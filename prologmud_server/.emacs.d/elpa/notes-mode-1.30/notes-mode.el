;;; notes-mode.el --- Indexing system for on-line note-taking

;;; Copyright (C) 1994-2007,2012  Free Software Foundation, Inc.

;; Author: <johnh@isi.edu>.

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
;; 

;;; Code:

(require 'notes-variables)
(require 'notes-aux)

(defvar notes-mode-hooks nil
  "Hooks to run when entering notes-mode.")
(defvar notes-load-mode-hooks nil
  "Hooks to run when entering notes-mode is loaded.")

(defconst notes-beginning-of-defun-regexp "^\\* .*\n\\-"
  "Regexp matching the beginning of notes section.")

(defvar notes-default-tab-binding (global-key-binding "\t")
  "Saved tab binding for notes-complete-subject.")
(defvar notes-default-return-binding (global-key-binding "\r")
  "Saved return binding for notes-electric-return.")


(defun notes-beginning-of-defun ()
  "Go to the beginning of a notes ``section''."
  (interactive)
  (let
      ((old-point (point)))
    (beginning-of-line)
    ;; handle starting on a title
    (if (and (looking-at notes-beginning-of-defun-regexp)
	     (/= (point) old-point))
	nil
      (goto-char old-point)
      (if (looking-at "^-")  ;; handle starting on the underline under a title
	  (forward-char 1))
      (re-search-backward notes-beginning-of-defun-regexp nil 'to-limit))))

(defun notes-end-of-defun ()
  "Go to the end of a notes ``section''."
  (interactive)
  (let ((regexp notes-beginning-of-defun-regexp))
    (if (looking-at regexp)
        (goto-char (match-end 0)))
    ;; Find next section and leave cursor at section beginning
    (if (re-search-forward regexp nil 'to-limit)
        (re-search-backward regexp 0 t)
      ;;(goto-char restore-point)
      )))

(defun notes-follow-link (which)
  "Go to the WHICH link for this topic.
WHICH is either \"next\" or \"prev\".
If there are no links for the current note,
we go to the last note based upon the index file."
  (let
      (beginning-of-note
       end-of-note
       (start-buffer (current-buffer))
       ;; We have to handle links in the same buffer,
       ;; so the following code figure out where we go
       ;; and returns it out of the save-excursion.
       ;; If we end up in another buffer, we let the save-excursion
       ;; leave the original buffer unchanged.  If we end up in
       ;; the same buffer, we need to go wherever we end up.
       ;; Can anyone suggest a better way?
       (end-buffer-and-point
	(save-excursion
	  (notes-end-of-defun)
	  (setq end-of-note (point))
	  (notes-beginning-of-defun)
	  (setq beginning-of-note (point))
	  (if (and (= beginning-of-note 1) (not (looking-at notes-beginning-of-defun-regexp)))
	      (progn
		;; When "above" the first note, search to end of first
		;; real note (otherwise end-of-note is just the start
		;; of the first real note and there are no links).
		(notes-end-of-defun)
		(notes-end-of-defun)
		(setq end-of-note (point))
		(goto-char beginning-of-note)))
	  (if (re-search-forward (concat "^"
					 (if (eq which 'next) "next" "prev")
					 ":[ ]+<") end-of-note t)
	      (progn ; link exists, just take it
		(beginning-of-line)
		(notes-w3-follow-link (point))
		(cons (current-buffer) (point)))
	    ;; No link; go through the index file.
	    (if (notes-goto-index-entry which)
		(let ((index-buffer (current-buffer)))
		  (notes-index-follow-link (point))
		  (bury-buffer index-buffer))
	      (error "No known notes in that direction.")
	      (bury-buffer (current-buffer)))
	    (cons (current-buffer) (point))))))
    ;; Check for going to the same buffer (and the save-excursion
    ;; undoing our work).
    (if (eq start-buffer (car end-buffer-and-point))
	(goto-char (cdr end-buffer-and-point)))))
       

(defun notes-follow-next-link ()
  "Go to the next link for this topic."
  (interactive)
  (notes-follow-link 'next))

(defun notes-follow-prev-link ()
  "Go to the previous link for this topic."
  (interactive)
  (notes-follow-link 'prev))

(defvar notes-complete-subject-abbrevs-alist
  '(("SP2010" "USC/Classes/CS551/SP2010")
    ("FA2011" "USC/Classes/CS551/FA2011"))
  "Alist of simple substitution of subjects.
If subject completion is requested, then subject that matches
the left-side of an alist value is replaced by the right-side value.")

(defun notes-complete-subject-abbrevs (key)
  "Handle abbreviations on notes SUBJECTS.
Currently this is just a hack."
  (let ((value (assoc key notes-complete-subject-abbrevs-alist)))
    (if value
	(car (cdr value))
      key)))
  

(defun notes-complete-subject ()
  "Complete the notes subject under point."
  (interactive)
  (let
      ((subject (save-excursion 
		  (beginning-of-line) 
		  (notes-extract-subject t)))
       old-completion-ignore-case
       full-subject)
    (if (not (and notes-mode-complete-subjects subject))
	(call-interactively notes-default-tab-binding)
      ;; Complete the title.
      (if (null notes-subject-table)
	  (save-excursion ;; FIXME: Why??
	    (find-file-noselect (expand-file-name "index" notes-dir))))
      ;; Do completion.
      ;; Run completer if it's loaded,
      ;; otherwise do our own thing.
      (setq completion-ignore-case t)
      (cond
       ((fboundp 'completer-complete-goto)
	(completer-complete-goto "^ \t\n\"" " " notes-subject-table nil))
       ;; NEEDSWORK:  should try other completers, too.
       (t   ;; Do our own completion.
	(setq full-subject (try-completion subject notes-subject-table)
	      subject (completing-read "Subject: "
				       notes-subject-table nil nil 
				       (if (stringp full-subject)
					   full-subject
					 subject)))
	(delete-region (line-beginning-position) (line-end-position))
	(insert "* " (notes-complete-subject-abbrevs subject))))
      	(setq completion-ignore-case old-completion-ignore-case))))

(defun notes-fix-prevnext-this-entry ()
  "Fix up the prev link for the current entry, if necessary.
Currently this code only handles brand new entries."
  ;; Contributed from Takashi Nishimoto <g96p0935@mse.waseda.ac.jp>.
  ;; Thanks!
  (interactive)
  (let ((subject (notes-extract-subject nil t))
        (this-url (notes-current-url))
        last-url)
    (with-current-buffer (find-file-noselect
                          (expand-file-name "index" notes-dir))
      (goto-char (point-min))
      (if (re-search-forward
           (concat "^" (regexp-quote subject) ":.* \\([0-9]+\\)$")
           (point-max) t) 
          (save-window-excursion
            (cond ((and (notes-w3-url
                         (notes-file-to-url (match-string 1) subject))
                        (re-search-forward "^next: " nil t)
                        (looking-at "<none>"))
		   (let
		       (pre-modified (buffer-modified-p))
		     (delete-char 6)
		     (insert this-url)
		     (setq last-url (notes-current-url))
		     (if (and (null pre-modified) 
			      (>= notes-electric-prevnext 2))
			 (save-buffer))))))))
    (if last-url
	(progn
	  (notes-beginning-of-defun)
	  (forward-line 2)
	  (if (not (looking-at "prev: "))
	      (insert "prev: " last-url "\n" "next: <none>\n\n")
	    (forward-line 3))))))

(defun notes-electric-return (arg)
  "* Return, underlining if we're on a subject."
  (interactive "*P")
  (if (let ((cur-point (point)))
	(save-excursion
	  (beginning-of-line)
	  (and (not (eq cur-point (point)))  ;; normal return if at b-o-ln
	       (notes-extract-subject t)))) 
      (progn (notes-underline-line)
	     (if notes-electric-prevnext
		 (notes-fix-prevnext-this-entry)))
    (call-interactively notes-default-return-binding)))

(defun notes-current-url ()
  "Return the notes-URL of the current entry around the current point."
  (let ((subject (notes-extract-subject nil t))
	(date (file-name-nondirectory buffer-file-name)))
    (concat "<file:///"
	    (abbreviate-file-name buffer-file-name)
	    (if subject (concat "#* " subject) "")
	    ">")))

(defun notes-current-url-as-kill ()
  "* Put the notes-URL of the current entry into the kill ring."
  (interactive)
  (kill-new (notes-current-url)))

(defun notes-goto-index-entry (&optional direction)
  "* Jump to the index entry corresponding to our current note entry.
If we're not in an entry, we leave you in the index file.
If the current date doesn't exist, error in DIRECTION.
Returns nil if on errors (no index; no date in DIRECTION),
otherwise the point of the hit."
  (interactive) 
  (let ((start-buffer (current-buffer))
	(subject (notes-extract-subject))  ; get subject if on it
	(date (if (null (buffer-file-name)) nil 
		(file-name-nondirectory (buffer-file-name)))))
    ;; Try and get the subject, either forward...
    (if (not subject)
	(save-excursion 
	  (notes-beginning-of-defun)
	  (setq subject (notes-extract-subject))))
    ;;    ...or backwards.
    (if (not subject)
	(save-excursion 
	  (notes-end-of-defun)
	  (setq subject (notes-extract-subject))))
    ;; Form and jump to the url for the index-entry.
    (if (and (notes-w3-url (concat notes-url-prefix
				   "index"
				   (if subject (concat "#" subject) ""))
			   nil t)
	     ;; Go to the current date, if any.
	     (notes-index-goto-date date direction))
	t
      nil)))

(defun notes-extract-subject (&optional relaxed search)
  "Extract the subject under the point in the current buffer.
If RELAXED, then accept non-underlined subjects.
If SEARCH we'll search back in the buffer for the nearest
subject title.

Returns nil if we're not on as subject."
  (save-match-data
    (cond
     ;; directly on a note
     ((or (looking-at notes-beginning-of-defun-regexp)
	  (and relaxed
	       (looking-at "^\\* ")))
      (save-excursion
	(let
	    ((start (+ (point) 2))
	     (end (progn (end-of-line) (point))))
	  (buffer-substring start end))))
     (search
      (save-excursion
	(notes-beginning-of-defun)
	(notes-extract-subject relaxed nil)))
     (t
      nil))))


;;;###autoload
(defun notes-underline-line ()
  "* Create a row of dashes as long as this line, or adjust the current underline."
  (interactive)
  ;; check to see if it's already underlined
  (if (save-excursion
	(forward-line 1)
	(looking-at "^[ \t]*--*$"))
      (notes-old-underline-line)
    (progn
      (notes-new-underline-line)
      (insert "\n\n"))))

(defun notes-new-underline-line ()
  "Underline a line with a row of dashes.  Move point after the dashes.
\\[notes-new-underline-line] reproduces leading spaces."
  (interactive)
  (let*
      ((bol (progn (beginning-of-line)
		   (point)))
       (bospaces (progn (skip-chars-forward " \t")
			(point)))
       (nospaces (- bospaces bol))
       (eol (progn (end-of-line)
		   (untabify bol (point))
		   (point))))
    (insert "\n" (buffer-substring bol bospaces))
    (insert-char ?- (- eol bospaces))))

(defun notes-old-underline-line ()
  "Replace the following line with a row of dashes.  Leave the point unchanged."
  (save-excursion
    (save-excursion
      (forward-line 1)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (notes-new-underline-line)))

(defun notes-mode-initialize-note-from-cache ()
  "Build a new note from the cache.  Return valid cache contents or nil."
  (save-excursion
    (let*
	((new-buffer (current-buffer))
	 (cache-file (expand-file-name "mknew.cache" notes-dir))
	 (buf (find-file cache-file))
	 magic-line
	 prev-file
	 this-file
	 cache-contents
	 m
	 (result
	  (if (and buf
		   (>= (count-lines (point-min) (point-max)) 3))
	      (progn
		;; If you know a more elegant way to extact the first
		;; three lines of a file, please let me know.
		(goto-char (point-min))
		(setq m (point))
		(forward-line 1)
		(setq magic-line (buffer-substring m (- (point) 1)))
		(setq m (point))
		(forward-line 1)
		(setq prev-file (buffer-substring m (- (point) 1)))
		(setq m (point))
		(forward-line 1)
		(setq this-file (buffer-substring m (- (point) 1)))
		(setq cache-contents (buffer-substring (point) (point-max)))
		(bury-buffer buf)
		;; is cache valid?
		(if
		    (and
		     (string-equal magic-line "mknew.cache 830494922")
		     (file-newer-than-file-p cache-file prev-file)
		     (string-equal (file-name-nondirectory this-file)
				   (file-name-nondirectory (buffer-file-name
							    new-buffer))))
		    cache-contents
		  nil))
	    nil)))
      ;; Kill the buffer to avoid "buf changed, reload?" warnings.
      (if buf
	  (kill-buffer buf))
      result)))

(defun notes-mode-initialize-note ()
  "Fill in an empty new note.
Create any directories as necessary.
Use the mknew cache if possible."
  (interactive)
  (let
      ((dir (directory-file-name (file-name-directory (buffer-file-name)))))
    (if (file-exists-p dir)
	t
      (make-directory dir t)
      (message "New intermediate directory created.")))
  (if notes-mode-initialization-program
      (let
	  ((cache-contents (notes-mode-initialize-note-from-cache)))
	(if cache-contents
	    (insert cache-contents)
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   (concat (expand-file-name notes-mode-initialization-program
                                     notes-bin-dir)
                   " '"
                   ;; FIXME: Use shell-quote-argument.
		   (buffer-file-name) "'") 't)))))
  

;;;
;;; encryption
;; requires "PEM - PGP Enhanced Messaging for GNU Emacs"
;; from Roy Frederick Busdiecker, III (Rick)
;; or mailcrypt 3.4.x or >=3.5.x
;;

(defvar notes-encryption-library 
  'mailcrypt
;  (cond
;   ((fboundp 'mc-encrypt-region) 'mailcrypt)
;   ((fboundp 'npgp:encrypt-region) 'npgp)
;   (t nil))
  "PGP library to use.")

(defvar notes-encryption-sub-library
  'gpg
  "Variant of mailcrypt to use (`pgp', `pgp50', or `gpg').")

(defvar notes-encryption-npgp-userid nil
  "PGP key for the current user.")

(defvar notes-encryption-npgp-key-id nil
  "Keyid of PGP key for the current user.
Useful if your \\[user-full-name] doesn't match a unique key.
Should have a leading 0x.")

(defun notes-encryption-npgp-userid ()
  "Return notes-encryption-userid, initializing it if necessary."
  (require 'pam)
  (if (and notes-encryption-userid
	   npgp:*pass-phrases*)
      notes-encryption-userid
    (setq notes-encryption-userid
	  (list
	   (if notes-encryption-key-id
	       (npgp:get-key-by-key-id notes-encryption-key-id)
	     (pam:read-name-key (user-full-name)))))))

(defun notes-encryption-mailcrypt-keyid ()
  "Do the right thing."
  (require 'mailcrypt)
  (cond
   ((eq notes-encryption-sub-library 'pgp)
    (cdr (mc-pgp-lookup-key mc-pgp-user-id)))
   ((eq notes-encryption-sub-library 'pgp50)
    (cdr (mc-pgp50-lookup-key mc-pgp50-user-id)))
   ((eq notes-encryption-sub-library 'gpg)
    (cdr (mc-gpg-lookup-key mc-gpg-user-id)))
   (t (error "notes-encryption-decrypt-region: no pgp sub-library."))))

(defun notes-encryption-load-mailcrypt ()
  (require 'mailcrypt)
  ;; ick ick ick this code needs to be cleaned up
  (cond
   ((null (eq notes-encryption-library 'mailcrypt))
    t)
   ((eq notes-encryption-sub-library 'pgp)
    (load-library "mc-pgp"))
   ((eq notes-encryption-sub-library 'pgp50)
    (load-library "mc-pgp5"))
   ((eq notes-encryption-sub-library 'gpg)
    (load-library "mc-gpg"))
   (t (error "notes-encryption-load-mailcrypt: no pgp sub-library."))))

(defun notes-encryption-decrypt-region (start end)
  (cond
   ((eq notes-encryption-library 'npgp)
    (require 'pam)
    (require 'npgp)
    (npgp:decrypt-region start end))
   ((eq notes-encryption-library 'mailcrypt)
    (notes-encryption-load-mailcrypt)
    (cond
     ((eq notes-encryption-sub-library 'pgp)
      (mc-pgp-decrypt-region start end))
     ((eq notes-encryption-sub-library 'pgp50)
      (mc-pgp50-decrypt-region start end))
     ((eq notes-encryption-sub-library 'gpg)
      (mc-gpg-decrypt-region start end))
     (t (error "notes-encryption-decrypt-region: no pgp sub-library."))))
   (t (error "notes-encryption-decrypt-region: no pgp library."))))

(defun notes-encryption-encrypt-region (start end)
  (cond
   ((eq notes-encryption-library 'npgp)
    (npgp:encrypt-region (notes-encryption-npgp-userid) start end))
   ((eq notes-encryption-library 'mailcrypt)
    (notes-encryption-load-mailcrypt)
    (let ((old-sign mc-pgp-always-sign)
	  old-comment recipients)
      (setq mc-pgp-always-sign 'never
	    recipients (list (notes-encryption-mailcrypt-keyid)))
      (cond
       ((eq notes-encryption-sub-library 'pgp)
	(setq old-comment mc-pgp-comment
	      mc-pgp-comment "")
	(mc-pgp-encrypt-region recipients start end
			       (notes-encryption-mailcrypt-keyid) nil)
	(setq mc-pgp-comment old-comment))
       ((eq notes-encryption-sub-library 'pgp50)
	(setq old-comment mc-pgp50-comment
	      mc-pgp50-comment "")
	(mc-pgp50-encrypt-region recipients start end
			       (notes-encryption-mailcrypt-keyid) nil)
	(setq mc-pgp50-comment old-comment))
       ((eq notes-encryption-sub-library 'gpg)
	(setq old-comment mc-gpg-comment
	      mc-gpg-comment "")
	(mc-gpg-encrypt-region recipients start end
			       (notes-encryption-mailcrypt-keyid) nil)
	(setq mc-gpg-comment old-comment))
       (t (error "notes-encryption-decrypt-region: no gpg sub-library.")))
      (setq mc-pgp-always-sign old-sign)))
   (t (error "notes-encryption-decrypt-region: no pgp library."))))

(defun notes-encrypt-note (prefix)
  "Encrypt the current note for the current user.  With PREFIX, start from point."
  (interactive "P")
  (save-excursion
    (let (start end)
      ;; Unless a prefix arg, start at beginning-of-note.
      (if prefix
	  nil
	(if (not (looking-at notes-beginning-of-defun-regexp))
	    (notes-beginning-of-defun))
	;; skip over the header
	(while (and (or (looking-at notes-beginning-of-defun-regexp)
			(looking-at "^-+$")
			(looking-at "^\\(prev\\|next\\): ")
			(looking-at "^[ \t]*$"))
		    (< (point) (point-max)))
	  (forward-line 1)))
      (setq start (point))
      ;; sanity check
      (if (re-search-forward "^-----BEGIN PGP MESSAGE" 
			     (progn 
			       (save-excursion 
				 (notes-end-of-defun) 
				 (point))) t)
	  (error "Note is already encrypted."))
      ;; find the end
      (notes-end-of-defun)
      (while (or (looking-at notes-beginning-of-defun-regexp)
		 (looking-at "^[ \t]*$"))
	(forward-line -1))
      (forward-line 1)
      (setq end (point))
      (notes-encryption-encrypt-region start end))))

(defun notes-decrypt-note ()
  "Decrypt the current note for the current user."
  (interactive)
  (save-excursion
    (if (not (looking-at notes-beginning-of-defun-regexp))
	(notes-beginning-of-defun))
    (if (null (re-search-forward "^-----BEGIN PGP" 
				 (progn 
				   (save-excursion 
				     (notes-end-of-defun) 
				     (point))) t))
	(error "Note is not encrypted."))
    (beginning-of-line)
    (let ((start (point)))
      (if (null (re-search-forward "^-----END PGP"
				   (progn
				     (save-excursion
				       (notes-end-of-defun)
				       (point))) t))
	  (error "Could not find end of encrypted note."))
      (forward-line)
      (beginning-of-line)
      (notes-encryption-decrypt-region start (point)))))


;;;
;;; notes or notes-index?
;;;
(defun notes-summarize-subject (regexp-subject &optional subject)
  "* Collect all of a subject."
  (interactive "P")
  (require 'notes-index-mode)
  (if (null subject)
      (cond
       ((eq major-mode 'notes-mode)
	(setq subject (notes-extract-subject nil t)))
       ((eq major-mode 'notes-index-mode)
	(setq subject (notes-index-extract-subject)))))
  (if (null subject)
   (error "notes-summarize-subject:  no subject specified or inferable."))
  (let
      ((buf (get-buffer-create (concat "*notes on " subject "*"))))
    (pop-to-buffer buf)
    (erase-buffer)
    (apply 'call-process (expand-file-name "catsubject" notes-bin-dir)
           nil buf t
	   (if regexp-subject
	       (list "-m" subject)
	     (list subject)))
    (notes-mode)))


;;;
;;; notes-rename-subject
;;;
(defun notes-rename-subject ()
  "* Rename the current subject.
Assumes working next/prev linkage between the entries."
  (interactive)
  (let ((subject (notes-extract-subject)))
    (condition-case nil
	(progn
	  (end-of-line)
	  (beginning-of-defun)
	  (if (not (looking-at "* "))
	      (error "confused"))
	  (forward-char 2)
	  (error "not yet done")
	  )
      (error nil))))


;;;
;;; notes-mode
;;;

(defvar notes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Random key-bindings.
    (define-key map "\M-\C-a" 'notes-beginning-of-defun)
    (define-key map "\M-\C-e" 'notes-end-of-defun)
    (define-key map "\C-c\C-d" 'notes-decrypt-note)
    (define-key map "\C-c\C-e" 'notes-encrypt-note)
    (define-key map "\C-c\r" 'notes-w3-follow-link)
    (define-key map "\C-c\C-p" 'notes-follow-prev-link)
    (define-key map "\C-c\C-n" 'notes-follow-next-link)
    (define-key map "\C-c\C-i" 'notes-goto-index-entry)
    (define-key map "\C-c\C-k" 'notes-current-url-as-kill)
    (define-key map "\C-c\C-s" 'notes-summarize-subject)
    (define-key map "\C-c-" 'notes-underline-line)
    ;; FIXME: Use completion-at-point-functions instead.
    (define-key map "\t" 'notes-complete-subject)
    ;; FIXME: Use post-self-insert-hook instead.
    (define-key map "\r" 'notes-electric-return)
    (define-key map "\n" 'notes-electric-return) ; a more common synonym
    (notes-platform-bind-mouse map 'S-mouse-2 'notes-w3-follow-link-mouse)
    map))

;;;###autoload
(define-derived-mode notes-mode indented-text-mode "Notes"
  "Enable notes-mode for a buffer.

Inside a notes buffer one can click on URLs and follow them to
other notes files.

See the file notes-variables.el for all customization options.
To change options, (require 'notes-variables) in your .emacs
and then change things.

Subjects in notes mode are lines beginning with an asterisk
and underlined with dashes.  Subjects can be completed 
with \\[notes-complete-subject] and are automatically underlined.

You may wish to add this code to your .emacs file:
    (add-to-list 'auto-mode-alist
  	(cons \"/9[0-9][0-9][0-9][0-9][0-9].?\\\\'\" 'notes-mode))
    (define-key global-map [?\\C-c ?n] 'notes-index-todays-link)
to automatically enter notes mode.

I have two suggestions for how to organize your notes files.
First, I collect my notes into a separate file per day.  (If you have
fewer notes, you may find once-per-week or month more suitable.)
Second, at the beginning of each file I have a subject \"* Today\".
Since every file has this subject, I can use its prev and next links
to easily move around the collection of files.

The key-bindings of this mode are:
\\{notes-mode-map}"
  (notes-platform-init)

  ;; Now set up the mode.
  (auto-fill-mode 1)

  ;; Imenu stuff.
  (set (make-local-variable 'imenu-prev-index-position-function)
       'notes-beginning-of-defun)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'notes-extract-subject)

  (set (make-local-variable 'font-lock-defaults)
       `(notes-font-lock-keywords
         t nil nil beginning-of-line))

  ;; Finally, try to fill in an empty note.
  (if (zerop (buffer-size))
      (notes-mode-initialize-note))

  ;; Enable outline-minor-mode (forcebly, in case someone already
  ;; has it in their text-mode hook).  Bug found by
  ;; Nils Ackermann <nils@nieback.de>.
  (if notes-use-outline-mode
      (outline-minor-mode 1)))




;;;

(run-hooks 'notes-mode-load-hooks)
(provide 'notes-mode)
;;; notes-mode.el ends here
