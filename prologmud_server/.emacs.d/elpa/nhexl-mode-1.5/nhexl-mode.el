;;; nhexl-mode.el --- Minor mode to edit files via hex-dump format  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2020  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: data
;; Version: 1.5
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements NHexl mode, a minor mode for editing files
;; in hex dump format.  The mode command is called `nhexl-mode'.
;;
;; This minor mode implements similar functionality to `hexl-mode',
;; but using a different implementation technique, which makes it
;; usable as a "plain" minor mode.  It works on any buffer, and does
;; not mess with the undo log or with the major mode.
;;
;; It also comes with:
;;
;; - `nhexl-nibble-edit-mode': a "nibble editor" minor mode.
;;   where the cursor pretends to advance by nibbles (4-bit) and the
;;   self-insertion keys let you edit the hex digits directly.
;;
;; - `nhexl-overwrite-only-mode': a minor mode to try and avoid moving text.
;;   In this minor mode, not only self-inserting keys overwrite existing
;;   text, but commands like `yank' and `kill-region' as well.
;;
;; - it overrides C-u to use hexadecimal, so you can do C-u a 4 C-f
;;   to advance by #xa4 characters.

;; Even though the hex addresses and hex data displayed by this mode aren't
;; actually part of the buffer's text (contrary to hexl-mode, for example,
;; they're only added to the display), you can search them with Isearch,
;; according to nhexl-isearch-hex-addresses and nhexl-isearch-hex-bytes.

;;;; Known bugs:
;;
;; - When the buffer is displayed in several windows, the "cursor" in the hex
;;   area only reflects one of the window-points.  Fixing this would be rather
;;   painful:
;;   - for every cursor, we need an extra overlay with the `window'
;;     property with its own `before-string'.
;;   - because that overlay won't *replace* the normal overlay (the one
;;     without the `window' property), we will need to *remove* that
;;     overlay (lest we get 2 before-strings) and replace it with N overlays
;;     with a `window' property (for all N other windows that don't have
;;     their cursor on this line).
;;   FWIW, the original `hexl-mode' has the same kind of problem.

;;;; Wishlist:

;; - An equivalent to hexl-mode's `hexl-bits'.
;; - Always reload the file with find-file-literally instead
;;   of editing the multibyte representation?

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'hexl)                         ;For faces.

(defgroup nhexl nil
  "Edit a file in a hex dump format."
  :group 'data)

(defcustom nhexl-line-width 16
  "Number of bytes per line."
  :type '(choice (integer :tag "Fixed width") (const :tag "Adjust to window" t)))

(defcustom nhexl-display-unprintables nil
  "If non-nil, display non-printable chars using the customary codes.
If nil, use just `.' for those chars instead of things like `\\NNN' or `^C'."
  :type 'boolean)

(defcustom nhexl-obey-font-lock t
  "If non-nil, faces will only be applied when font-lock is enabled.
Otherwise they are applied unconditionally."
  :type 'boolean)

(defcustom nhexl-silently-convert-to-unibyte nil
  "If non-nil `nhexl-mode' won't ask before converting the buffer to unibyte."
  :type 'boolean)

(defcustom nhexl-isearch-hex-addresses t
  "If non-nil, hex search terms will look for matching addresses."
  :type 'boolean)

(defcustom nhexl-isearch-hex-bytes t
  "If non-nil, hex search terms will look for matching bytes."
  :type 'boolean)

(defcustom nhexl-isearch-hex-highlight t
  "If non-nil, nhexl will highlight Isearch matches in the hex areas as well."
  :type 'boolean)

(defcustom nhexl-group-size (max 1 (/ hexl-bits 8))
  "Number of bytes in each group.
Groups are separated by spaces."
  :type 'integer)

(defcustom nhexl-separate-line nil
  ;; FIXME: This var is not taken into account when auto-sizing the
  ;; line-width!
  "If non-nil, put the ascii area below the hex, on a separate line."
  :type 'boolean)

(defvar nhexl--display-table
  (let ((dt (make-display-table)))
    (unless nhexl-display-unprintables
      (dotimes (i 128)
        (when (> (char-width i) 1)
          (setf (aref dt i) [?.])))
      (dotimes (i 128)
        (setf (aref dt (unibyte-char-to-multibyte (+ i 128))) [?.])))
    ;; (aset dt ?\n [?␊])
    (aset dt ?\t [?␉])
    dt))

(defvar-local nhexl--saved-vars nil)

;;;; Nibble editing minor mode

;; FIXME: Region highlighting in this minor mode should highlight the hex area
;;   rather than only the ascii area!
;; FIXME: Kill&yank in this minor mode should work on the hex representation
;;   of the buffer's content (and should obey overwrite-mode)!

(defvar nhexl-nibble-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'nhexl-nibble-self-insert)
    (define-key map [remap right-char] #'nhexl-nibble-forward)
    (define-key map [remap forward-char] #'nhexl-nibble-forward)
    (define-key map [remap left-char] #'nhexl-nibble-backward)
    (define-key map [remap backward-char] #'nhexl-nibble-backward)
    map))

;; FIXME: Reuben Thomas pointed out that the user may not think of it as
;; "editing nibbles" but "editing the hex codes" instead.
;; Maybe we should rename `nhexl-nibble-edit-mode'?
(defalias 'nhexl-hex-edit-mode #'nhexl-nibble-edit-mode)
(define-minor-mode nhexl-nibble-edit-mode
  "Minor mode to edit the hex nibbles in `nhexl-mode'."
  :global nil
  (if nhexl-nibble-edit-mode
      (setq-local cursor-type 'hbar)
    (kill-local-variable 'cursor-type))
  (nhexl--refresh-cursor))

(defvar-local nhexl--nibbles nil
  "Nibble state of the various `point's.
List of elements of the form (WINDOW OFFSET POINT TICKS),
where WINDOW can be nil (for the `point' of the buffer itself);
OFFSET is the nibble-position within the byte at POINT (0 = leftmost);
and TICKS is the `buffer-chars-modified-tick' for which this was valid.")

(defun nhexl--nibble (&optional pos)
  (let ((cwin (if (eq (current-buffer) (window-buffer)) (selected-window)))
        (data ()))
    (dolist (n nhexl--nibbles)
      (let ((nwin (car n)))
        (cond
         ((eq cwin nwin) (setq data n))
         ((eq (current-buffer) (window-buffer nwin)) nil)
         (t (setq nhexl--nibbles (delq n nhexl--nibbles))))))
    (or (and (eq (or pos (point)) (nth 2 data))
             (eq (buffer-chars-modified-tick) (nth 3 data))
             (nth 1 data))
        (progn
          (setq nhexl--nibbles (delq data nhexl--nibbles))
          0))))

(defun nhexl--nibble-set (n)
  (let* ((cwin (if (eq (current-buffer) (window-buffer)) (selected-window)))
         (data (assq cwin nhexl--nibbles)))
    (unless data
      (push (setq data (list cwin)) nhexl--nibbles))
    (setcdr data (list n (point) (buffer-chars-modified-tick)))))

(defsubst nhexl--line-width ()
  (if (integerp nhexl-line-width) nhexl-line-width 16))

(defun nhexl--nibble-max (&optional char)
  (unless char (setq char (following-char)))
  (if (< char 256) 1
    (let ((i 1))
      (setq char (/ char 256))
      (while (> char 0)
        (setq char (/ char 16))
        (setq i (1+ i)))
      i)))

(defun nhexl-nibble-forward ()
  "Advance by one nibble."
  (interactive)
  (let ((nib (nhexl--nibble)))
    (if (>= nib (nhexl--nibble-max))
        (forward-char 1)
      (nhexl--nibble-set (1+ nib))
      (nhexl--refresh-cursor))))

(defun nhexl-nibble-backward ()
  "Advance by one nibble."
  (interactive)
  (let ((nib (nhexl--nibble)))
    (if (> nib 0)
        (progn
          (nhexl--nibble-set (1- nib))
          (nhexl--refresh-cursor))
      (backward-char 1)
      (nhexl--nibble-set (nhexl--nibble-max)))))

(defun nhexl-nibble-self-insert ()
  "Overwrite current nibble with the hex character you type."
  (interactive)
  (let* ((max (nhexl--nibble-max))
         (nib (min max (nhexl--nibble)))
         (char (if (and (not overwrite-mode) (= nib 0)) 0 (following-char)))
         (hex (format "%02x" char))
         (nhex (concat (substring hex 0 nib)
                       (string last-command-event)
                       (substring hex (1+ nib))))
         (nchar (string-to-number nhex 16)))
    (insert nchar)
    (unless (or (eobp)
                (and (not overwrite-mode) (= nib 0)))
      (delete-char 1))
    (if (= max nib) nil
      (backward-char 1)
      (nhexl--nibble-set (1+ nib)))))

;;;; No insertion/deletion minor mode

;; FIXME: To make it work more generally, we should hook into
;; after-change-function, but we can't work directly from there because
;; it's called at too fine a grain (an overwrite is actually an
;; insertion+deletion and will run after-change-function, twice).

(defvar nhexl-overwrite-clear-byte ?\000
  "Byte to use to replace deleted content.")

(defvar nhexl-overwrite-only-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap yank] #'nhexl-overwrite-yank)
    (define-key map [remap yank-pop] #'nhexl-overwrite-yank-pop)
    (define-key map [remap kill-region] #'nhexl-overwrite-kill-region)
    (define-key map [remap delete-char] #'nhexl-overwrite-delete-char)
    (define-key map [remap backward-delete-char-untabify]
      #'nhexl-overwrite-backward-delete-char)
    (define-key map [remap backward-delete-char]
      #'nhexl-overwrite-backward-delete-char)
    map))

(defun nhexl-overwrite-backward-delete-char (&optional arg)
  "Delete ARG chars backward by overwriting them.
Uses `nhexl-overwrite-clear-byte'."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (nhexl-overwrite-delete-char (- arg))
    (forward-char (- arg))
    (save-excursion
      (insert-char nhexl-overwrite-clear-byte arg)
      (delete-char arg))))

(defun nhexl-overwrite-delete-char (&optional arg)
  "Delete ARG chars forward by overwriting them.
Uses `nhexl-overwrite-clear-byte'."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (nhexl-overwrite-backward-delete-char (- arg))
    (insert-char nhexl-overwrite-clear-byte arg)
    (delete-char arg)))

(defun nhexl-overwrite-kill-region (beg end &optional region)
  "Kill the region, replacing it with `nhexl-overwrite-clear-byte'."
  (interactive (list (mark) (point) 'region))
  (copy-region-as-kill beg end region)
  (barf-if-buffer-read-only)
  (pcase-dolist (`(,beg . ,end)
                 (if region (funcall region-extract-function 'bounds)
                   (list beg end)))
    (goto-char beg)
    (nhexl-overwrite-delete-char (- end beg))))

(defun nhexl-overwrite--yank-wrapper (fun)
  ;; FIXME? doesn't work when yanking things like rectangles.
  (let ((orig-size (buffer-size)))
    (funcall fun)
    (let* ((inserted (- (buffer-size) orig-size))
           (deleted (delete-and-extract-region
                     (point)
                     (min (point-max) (+ (point) inserted)))))
      (unless yank-undo-function
        (setq yank-undo-function #'delete-region))
      (add-function :before yank-undo-function
                    (lambda (_beg end)
                      (save-excursion
                        (goto-char end)
                        (insert deleted)))))))

(defun nhexl-overwrite-yank (&optional arg)
  "Like `yank' but overwriting existing text."
  (interactive "*P")
  (nhexl-overwrite--yank-wrapper (lambda () (yank arg))))

(defun nhexl-overwrite-yank-pop (&optional arg)
  "Like `yank-pop' but overwriting existing text."
  (interactive "*P")
  (nhexl-overwrite--yank-wrapper (lambda () (yank-pop arg))))

(defvar-local nhexl--overwrite-save-settings nil)

(define-minor-mode nhexl-overwrite-only-mode
  "Minor mode where text is only overwritten.
Insertion/deletion is avoided where possible and replaced by overwriting
existing text, if needed with `nhexl-overwrite-clear-byte'."
  :lighter nil
  (cond
   (nhexl-overwrite-only-mode
    (push (cons 'overwrite-mode overwrite-mode)
          nhexl--overwrite-save-settings)
    (setq-local overwrite-mode 'overwrite-mode-binary)
    (setq-local overwrite-mode-binary " OnlyOvwrt"))
   (t
    (pcase-dolist (`(,var . ,val)
                   (prog1 nhexl--overwrite-save-settings
                     (setq nhexl--overwrite-save-settings nil)))
      (set var val))
    (kill-local-variable 'overwrite-mode-binary))))

;;;; Main minor mode

(defvar nhexl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `next-line' and `previous-line' work correctly, but they take ages in
    ;; large buffers and allocate an insane amount of memory, so the GC is
    ;; constantly triggered.
    ;; So instead we just override them with our own custom-tailored functions
    ;; which don't have to work nearly as hard to figure out where's the
    ;; next line.
    ;; FIXME: It would also be good to try and improve `next-line' and
    ;; `previous-line' for this case, tho it is pretty pathological for them.
    (define-key map [remap next-line] #'nhexl-next-line)
    (define-key map [remap previous-line] #'nhexl-previous-line)
    (define-key map [remap move-end-of-line] #'nhexl-move-end-of-line)
    (define-key map [remap move-beginning-of-line]
      #'nhexl-move-beginning-of-line)
    ;; Just as for line movement, scrolling movement could/should work as-is
    ;; but benefit from an ad-hoc implementation.
    (define-key map [remap scroll-up-command] #'nhexl-scroll-up)
    (define-key map [remap scroll-down-command] #'nhexl-scroll-down)
    (define-key map [remap mouse-set-point] #'nhexl-mouse-set-point)
    (define-key map [remap mouse-drag-region] #'nhexl-mouse-drag-region)
    (define-key map [remap mouse-set-region] #'nhexl-mouse-set-region)
    ;; FIXME: Should we really make it hard to use non-binary `overwrite-mode'?
    ;; Or should we go even further and remap it to
    ;; `nhexl-overwrite-only-mode'?
    (define-key map [remap overwrite-mode] #'binary-overwrite-mode)
    ;; FIXME: Find a key binding for nhexl-nibble-edit-mode!
    map))

(defvar-local nhexl--point nil)

;;;###autoload
(define-minor-mode nhexl-mode
  "Minor mode to edit files via hex-dump format"
  :lighter (" NHexl" (nhexl-nibble-edit-mode "/ne"))
  (dolist (varl (prog1 nhexl--saved-vars
                  (kill-local-variable 'nhexl--saved-vars)))
    (set (make-local-variable (car varl)) (cdr varl)))

  (if (not nhexl-mode)
      (progn
        (jit-lock-unregister #'nhexl--jit)
        (remove-hook 'after-change-functions #'nhexl--change-function 'local)
        (remove-hook 'post-command-hook #'nhexl--post-command 'local)
        (if (>= emacs-major-version 27)
            (remove-hook 'window-size-change-functions #'nhexl--window-size-change t)
          (remove-hook 'window-configuration-change-hook
                       #'nhexl--window-config-change t)
          (remove-hook 'window-size-change-functions #'nhexl--window-size-change))
        (remove-function (local 'isearch-search-fun-function)
                         #'nhexl--isearch-search-fun)
        ;; FIXME: This conflicts with any other use of `display'.
        (with-silent-modifications
          (put-text-property (point-min) (point-max) 'display nil))
        (remove-overlays (point-min) (point-max) 'nhexl t))

    (when (and enable-multibyte-characters
               ;; No point changing to unibyte in a pure-ASCII buffer.
               (not (= (position-bytes (point-max)) (point-max)))
               (not (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (re-search-forward "[^[:ascii:]\200-\377]" nil t))))
               ;; We're in a multibyte buffer which only contains bytes,
               ;; so we could advantageously convert it to unibyte.
               (or nhexl-silently-convert-to-unibyte
                   (y-or-n-p "Make buffer unibyte? ")))
      (set-buffer-multibyte nil))
                   
    (unless (local-variable-p 'nhexl--saved-vars)
      (dolist (var '(buffer-display-table buffer-invisibility-spec
                     overwrite-mode header-line-format word-wrap))
        (push (cons var (symbol-value var)) nhexl--saved-vars)))
    (setq nhexl--point (point))
    ;; Word-wrap doesn't make much sense together with nhexl-mode and
    ;; the display-engine tends to suffer unduly if it's enabled.
    (setq-local word-wrap nil)
    (setq-local header-line-format '(:eval (nhexl--header-line)))
    (binary-overwrite-mode 1)
    (setq-local buffer-invisibility-spec ())
    (setq-local buffer-display-table nhexl--display-table)
    (jit-lock-register #'nhexl--jit)
    (add-hook 'change-major-mode-hook (lambda () (nhexl-mode -1)) nil 'local)
    (add-hook 'post-command-hook #'nhexl--post-command nil 'local)
    (add-hook 'after-change-functions #'nhexl--change-function nil 'local)
    (if (>= emacs-major-version 27)
        (add-hook 'window-size-change-functions #'nhexl--window-size-change nil t)
      (add-hook 'window-configuration-change-hook
                #'nhexl--window-config-change nil 'local)
      (add-hook 'window-size-change-functions #'nhexl--window-size-change))
    (add-function :around (local 'isearch-search-fun-function)
                  #'nhexl--isearch-search-fun)
    ;; FIXME: We should delay this to after running the minor-mode hook.
    (when (and (eq t (default-value 'nhexl-line-width))
               (eq (current-buffer) (window-buffer)))
      (nhexl--adjust-to-width))))

(defun nhexl-next-line (&optional arg)
  "Move cursor vertically down ARG lines."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (nhexl-previous-line (- arg))
    (let ((nib (nhexl--nibble)))
      (forward-char (* arg (nhexl--line-width)))
      (nhexl--nibble-set nib))))

(defun nhexl-previous-line (&optional arg)
  "Move cursor vertically up ARG lines."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (nhexl-next-line (- arg))
    (let ((nib (nhexl--nibble)))
      (backward-char (* arg (nhexl--line-width)))
      (nhexl--nibble-set nib))))

(defun nhexl-move-beginning-of-line (&optional arg)
  "Move to beginning of the hex line that lies ARG - 1 hex lines ahead."
  (interactive "p")
  (unless arg (setq arg 1))
  (nhexl-next-line (- arg 1))
  (backward-char (mod (- (point) 1) nhexl-line-width)))

(defun nhexl-move-end-of-line (&optional arg)
  "Move to end of the hex line that lies ARG - 1 hex lines ahead."
  (interactive "p")
  (unless arg (setq arg 1))
  (nhexl-next-line (- arg 1))
  (forward-char (- nhexl-line-width 1 (mod (- (point) 1) nhexl-line-width))))

(defun nhexl-scroll-down (&optional arg)
  "Scroll text of selected window down ARG lines; or near full screen if no ARG."
  (interactive "P")
  (unless arg
    ;; Magic extra 2 lines: 1 line to account for the header-line, and a second
    ;; to account for the extra empty line that somehow ends up being there
    ;; pretty much all the time right below the header-line :-(
    (setq arg (max 1 (- (window-text-height) next-screen-context-lines 2))))
  (cond
   ((< arg 0) (nhexl-scroll-up (- arg)))
   ((eq arg '-) (nhexl-scroll-up nil))
   ((bobp) (scroll-down arg))			; signal error
   (t
    (let* ((ws (window-start))
           (nws (- ws (* (nhexl--line-width) arg))))
      (if (eq ws (point-min))
          (if scroll-error-top-bottom
              (nhexl-previous-line arg)
            (scroll-down arg))
        (nhexl-previous-line arg)
        (set-window-start nil (max (point-min) nws)))))))

(defun nhexl-scroll-up (&optional arg)
  "Scroll text of selected window up ARG lines; or near full screen if no ARG."
  (interactive "P")
  (unless arg
    ;; Magic extra 2 lines: 1 line to account for the header-line, and a second
    ;; to account for the extra empty line that somehow ends up being there
    ;; pretty much all the time right below the header-line :-(
    (setq arg (max 1 (- (window-text-height) next-screen-context-lines 2))))
  (cond
   ((< arg 0) (nhexl-scroll-down (- arg)))
   ((eq arg '-) (nhexl-scroll-down nil))
   ((eobp) (scroll-up arg))			; signal error
   (t
    (let* ((ws (window-start))
           (nws (+ ws (* (nhexl--line-width) arg))))
      (if (pos-visible-in-window-p (point-max))
          (if scroll-error-top-bottom
              (nhexl-next-line arg)
            (scroll-up arg))
        (nhexl-next-line arg)
        (set-window-start nil (min (point-max) nws)))))))

;; If we put the LFs in the before-string, we get a spurious empty
;; line at the top of the window (bug#31276), so we put the LFs
;; via a `display' property by default, but it's a bit complicated.
(eval-and-compile
  (defvar nhexl--put-LF-in-string nil))

(defun nhexl--posn-hexadjust (posn)
  "Adjust POSN when clicking on the hex area.
Return the corresponding nibble, if applicable."
  ;; When clicking in the hex area, (nth 1 posn) contains the first position
  ;; covered by the before-string, and (nth 5 posn) as well.  Improve this by
  ;; setting nth-5 (the one used by `posn-point') to the closest buffer
  ;; position corresponding to the hex on which we clicked.
  (let* ((str-data (posn-string posn))
         (base-pos (nth 1 posn))
         (addr-offset (eval-when-compile
                        (+ (if nhexl--put-LF-in-string 1 0)
                           9           ;for "<address>:"
                           1))))       ;for the following (stretch)space
    ;; (message "NMSP: strdata=%S" str-data)
    (when (and (consp str-data) (stringp (car str-data)) (integerp base-pos)
               (integerp (cdr str-data)) (> (cdr str-data) addr-offset))
      (let* ((hexchars (- (cdr str-data) addr-offset))
             ;; FIXME: Calculations here go wrong in the presence of
             ;; chars with code > 255.
             (hex-no-spaces (- hexchars (/ (1+ hexchars) 5)))
             (bytes (min (/ hex-no-spaces 2)
                         ;; Bound, for clicks between the hex and ascii areas.
                         (1- (nhexl--line-width))))
             (newpos (min (+ base-pos bytes) (point-max))))
        (setf (nth 5 posn) newpos)
        (let* ((nibble (- hex-no-spaces (* bytes 2))))
          (min nibble 1))))))

(defun nhexl-mouse-set-point (event)
  "Move point to the position clicked on with the mouse."
  (interactive "e")
  (let* ((nibble (nhexl--posn-hexadjust (event-end event))))
    (call-interactively #'mouse-set-point)
    (when (and nibble nhexl-nibble-edit-mode)
      (nhexl--nibble-set nibble)
      (nhexl--refresh-cursor))))

(defun nhexl-mouse-drag-region (event)
  "Set the region to the text that the mouse is dragged over."
  (interactive "e")
  (nhexl--posn-hexadjust (event-start event))
  (call-interactively #'mouse-drag-region))

(defun nhexl-mouse-set-region (event)
  "Set the region to the text dragged over, and copy to kill ring."
  (interactive "e")
  (nhexl--posn-hexadjust (event-start event))
  (nhexl--posn-hexadjust (event-end event))
  (call-interactively #'mouse-set-region))

(defun nhexl--change-function (beg end len)
  ;; Round modifications up-to the hexl-line length since nhexl--jit will need
  ;; to modify the overlay that covers that text.
  (let* ((zero (save-restriction (widen) (point-min)))
         (lw (nhexl--line-width))
         (from (max (point-min)
                    (+ zero (* (truncate (- beg zero) lw) lw))))
         (to (min (point-max)
                  (+ zero (* (ceiling (- end zero) lw)
                             lw)))))
    (with-silent-modifications    ;Don't store this change in buffer-undo-list!
      (put-text-property from to 'fontified nil)))
  ;; Also make sure the tail's addresses are refreshed when
  ;; text is inserted/removed.
  (when (/= len (- end beg))
    (with-silent-modifications    ;Don't store this change in buffer-undo-list!
      (put-text-property beg (point-max) 'fontified nil))))

(defun nhexl--flush ()
  (save-restriction
    (widen)
    (nhexl--change-function (point-min) (point-max) (buffer-size))))

(defvar nhexl--overlay-counter 1000)
(make-variable-buffer-local 'nhexl--overlay-counter)

(defun nhexl--debug-count-ols ()
  (let ((i 0))
    (dolist (ol (overlays-in (point-min) (point-max)))
      (when (overlay-get ol 'nhexl) (cl-incf i)))
    i))

(defun nhexl--flush-overlays (buffer)
  (with-current-buffer buffer
    (kill-local-variable 'nhexl--overlay-counter)
    ;; We've created many overlays in this buffer, which can slow
    ;; down operations significantly.  Let's flush them.
    ;; An easy way to flush them is
    ;;   (remove-overlays min max 'nhexl t)
    ;;   (put-text-property min max 'fontified nil)
    ;; but if the visible part of the buffer requires more than
    ;; nhexl--overlay-counter overlays, then we'll inf-loop.
    ;; So let's be more careful about removing overlays.
    (let ((windows (get-buffer-window-list nil nil t))
          (lw (nhexl--line-width))
          (start (point-min))
          (zero (save-restriction (widen) (point-min)))
          (debug-count (nhexl--debug-count-ols)))
      (with-silent-modifications
        (while (< start (point-max))
          (let ((end (point-max)))
            (dolist (window windows)
              (cond
               ((< start (1- (window-start window)))
                (setq end (min (1- (window-start window)) end)))
               ((< start (1+ (window-end window)))
                (setq start (1+ (window-end window))))))
            ;; Round to multiple of lw.
            (setq start (+ zero (* (ceiling (- start zero) lw) lw)))
            (setq end (+ zero (* (truncate (- end zero) lw) lw)))
            (when (< start end)
              (remove-overlays start end 'nhexl t)
              (put-text-property start end 'fontified nil)
              (setq start (+ end lw))))))
      (let ((debug-new-count (nhexl--debug-count-ols)))
        (message "Flushed %d overlays, %d remaining"
                 (- debug-count debug-new-count) debug-new-count)))))

(defun nhexl--make-line (from next zero &optional point)
  (let* ((nextpos (min next (point-max)))
         (lw (nhexl--line-width))
         (bufstr (buffer-substring from nextpos))
         (prop (if nhexl-obey-font-lock 'font-lock-face 'face))
         (i -1)
         (s (concat
             (if nhexl--put-LF-in-string (unless (eq zero from) "\n"))
             (format (if (or (null point)
                             (< point from)
                             (>= point next))
                         (propertize "%08x:" prop 'hexl-address-region)
                       ;; The `face' property overrides the `font-lock-face'
                       ;; property (instead of being combined), but we want the
                       ;; `highlight' face to be present regardless of
                       ;; font-lock-mode, so we can't use font-lock-face.
                       (propertize "%08x:" 'face
                                   (if (or font-lock-mode
                                           (not nhexl-obey-font-lock))
                                       '(highlight hexl-address-region default)
                                     'highlight)))
                     (- from zero))
             (eval-when-compile (propertize " " 'display '(space :align-to 12)))
             (mapconcat (lambda (c)
                          (setq i (1+ i))
                          ;; FIXME: In multibyte buffers, do something clever
                          ;; about non-ascii chars.
                          (let ((s (format "%02x" c))
                                face)
                            (when (and isearch-mode
                                       (memq (setq face (get-char-property
                                                         (+ i from) 'face))
                                             '(lazy-highlight isearch)))
                              (put-text-property 0 (length s) 'face
                                                 `(,face default) s))
                            (when (and point (eq point (+ from i)))
                              (if nhexl-nibble-edit-mode
                                  (let ((nib (min (nhexl--nibble point)
                                                  (1- (length s)))))
                                    (put-text-property nib (1+ nib)
                                                       'face '(highlight default)
                                                       s))
                                (put-text-property 0 (length s)
                                                   'face '(highlight default)
                                                   s)))
                            (if (not (zerop (mod (1+ i) nhexl-group-size)))
                                ;; FIXME: If this char and the next are both
                                ;; covered by isearch highlight, we should
                                ;; also highlight the space.
                                s (concat s " "))))
                        bufstr
                        "")
             (if (> next nextpos)
                 (make-string (+ (/ (1+ (- next nextpos)) nhexl-group-size)
                                 (* (- next nextpos) 2))
                              ?\s))
             (if nhexl-separate-line
                 (concat "\n"
                         (propertize "  " 'display
                           `(space :align-to 12)))
               (propertize "  " 'display
                           `(space :align-to
                                   ,(+ (* lw 2)                ;digits
                                       (/ lw nhexl-group-size) ;spaces
                                       12 3)))))))              ;addr + borders
    (font-lock-append-text-property 0 (length s) prop 'default s)
    ;; If the first char of the text has a button (e.g. it's part of
    ;; a hyperlink), clicking in the hex part of the display might signal
    ;; an error because it thinks we're clicking on the hyperlink.
    ;; So override the relevant properties.
    (put-text-property 0 (length s) 'keymap (make-sparse-keymap) s)
    (put-text-property 0 (length s) 'follow-link #'ignore s)
    ;; Override any `category' property that might otherwise be inherited from
    ;; the text (e.g. that of some button).
    ;; FIXME: This doesn't have the intended effect!
    (put-text-property 0 (length s) 'category t s)
    s))

(defun nhexl--jit (from to)
  (let ((zero (save-restriction (widen) (point-min)))
        (lw (nhexl--line-width)))
    (setq from (max (point-min)
                    (+ zero (* (truncate (- from zero) lw) lw))))
    (setq to (min (point-max)
                  (+ zero (* (ceiling (- to zero) lw) lw))))
    (remove-overlays from to 'nhexl t)
    (remove-text-properties from to '(display))
    (save-excursion
      (goto-char from)
      (while (search-forward "\n" to t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'display (copy-sequence "␊"))))
    (while (< from to)

      (cl-decf nhexl--overlay-counter)
      (when (and (= nhexl--overlay-counter 0)
                 ;; If the user enabled jit-lock-stealth fontification, then
                 ;; removing overlays is just a waste since
                 ;; jit-lock-stealth will restore them anyway.
                 (not jit-lock-stealth-time))
        ;; (run-with-idle-timer 0 nil #'nhexl--flush-overlays (current-buffer))
        )
      
      (let* ((next (+ from lw))
             (ol (make-overlay from next))
             (s (nhexl--make-line from next zero nhexl--point))
             (c (char-before next)))
        (when nhexl-separate-line
          (dotimes (i (- (min (point-max) next) from 1))
            (let ((ol (make-overlay (+ from i) (+ from i 1))))
              (overlay-put ol 'nhexl t)
              (overlay-put ol 'after-string
                           (propertize " " 'display
                                       `(space :align-to
                                         ,(+ (* (1+ i) 2)                ;digits
                                             (/ (1+ i) nhexl-group-size) ;spaces
                                             12)))))))
        (unless (or nhexl--put-LF-in-string (>= next (point-max)))
          ;; Display tables aren't applied to strings in `display' properties,
          ;; so we have to mimick it by hand.
          (let ((cdisplay (aref nhexl--display-table
                                (if enable-multibyte-characters c
                                  (unibyte-char-to-multibyte c)))))
            (put-text-property (1- next) next
                               'display (concat
                                         (string (cond
                                                  ((eq c ?\n) ?␊)
                                                  (cdisplay (aref cdisplay 0))
                                                  (t c)))
                                         ;; Explicit set a `default' face
                                         ;; lest it gets nhexl-ascii-region.
                                         (eval-when-compile
                                           (propertize "\n" 'face 'default))))))
        (overlay-put ol 'nhexl t)
        (overlay-put ol (if nhexl-obey-font-lock 'font-lock-face 'face)
                     'hexl-ascii-region)
        ;; Make sure these overlays have less priority than that of (say)
        ;; the region highlighting (since they're rather small).  Another way
        ;; to do it would be to add an overlay over the whole buffer with the
        ;; `face' property.
        (overlay-put ol 'priority most-negative-fixnum)
        (overlay-put ol 'before-string s)
        ;; (overlay-put ol 'after-string "\n")
        (setq from next)))
    ))

(defun nhexl--refresh-cursor (&optional pos)
  (unless pos (setq pos (point)))
  (let* ((zero (save-restriction (widen) (point-min)))
         (lw (nhexl--line-width))
         (n (truncate (- pos zero) lw))
         (from (max (point-min) (+ zero (* n lw))))
         (to (min (point-max) (+ zero (* (1+ n) lw)))))
    (with-silent-modifications
      (put-text-property from to 'fontified nil))))

(defun nhexl--header-line ()
  ;; FIXME: merge with nhexl--make-line?
  ;; FIXME: Memoize last line to avoid recomputation!
  (let* ((zero (save-restriction (widen) (point-min)))
         (lw (nhexl--line-width))
         (text
          (let ((tmp ()))
            (dotimes (i lw)
              (setq i (logand i #xf))
              (push (if (< i 10) (+ i ?0) (+ i -10 ?a)) tmp))
            (apply #'string (nreverse tmp))))
         (pos (1+ (mod (- (point) zero) lw)))
         (i 0))
    (put-text-property (1- pos) pos 'face 'highlight text)
    (concat
     (eval-when-compile (propertize " " 'display '(space :align-to 0)))
     "Address:"
     (eval-when-compile (propertize " " 'display '(space :align-to 12)))
     (mapconcat (lambda (c)
                  (setq i (1+ i))
                  (let ((s (string c c)))
                    (when (eql i pos)
                      (if nhexl-nibble-edit-mode
                          (let ((nib (min (nhexl--nibble (point))
                                          (1- (length s)))))
                            (put-text-property nib (1+ nib)
                                               'face 'highlight
                                               s))
                        (put-text-property 0 (length s)
                                           'face 'highlight
                                           s)))
                    (if (not (zerop (mod i nhexl-group-size)))
                        s
                      (concat
                       s (propertize " " 'display
                                     `(space :align-to
                                             ,(+ (* i 2)                ;digits
                                                 (/ i nhexl-group-size) ;spaces
                                                 12)))))))              ;addr
                text
                "")
     (unless nhexl-separate-line
       (concat
        (propertize "  " 'display
                    `(space :align-to
                            ,(+ (* lw 2)                ;digits
                                (/ lw nhexl-group-size) ;spaces
                                12 3)))                 ;addr + border
        text)))))
  

(defun nhexl--post-command ()
  (when (/= (point) nhexl--point)
    (let ((zero (save-restriction (widen) (point-min)))
          (lw (nhexl--line-width))
          (oldpoint nhexl--point))
      (setq nhexl--point (point))
      (nhexl--refresh-cursor)
      ;; (nhexl--jit (point) (1+ (point)))
      (if (/= (truncate (- (point) zero) lw)
              (truncate (- oldpoint zero) lw))
          (nhexl--refresh-cursor oldpoint)))))

(defun nhexl--isearch-match-hex-bytes (string bound noerror)
  ;; "57a" can be taken as "57a." or ".57a", but we currently
  ;; only handle "57a."
  ;; TODO: Maybe we could support hex regexps as well?
  (let ((i 0)
        (chars ()))
    (while (< (1+ i) (length string))
      (push (string-to-number (substring string i (+ i 2)) 16)
            chars)
      (setq i (+ i 2)))
    (let* ((base (regexp-quote (apply #'unibyte-string (nreverse chars))))
           (re
            (concat (if (>= i (length string))
                        base
                      (cl-assert (= (1+ i) (length string)))
                      (let ((nibble (string-to-number (substring string i) 16)))
                        ;; FIXME: if one of the two bounds is a special char
                        ;; like `]` or `^' we can get into trouble!
                        (concat base
                                (unibyte-string ?\[ (* 16 nibble) ?-
                                                   (+ 15 (* 16 nibble)) ?\]))))
                    ;; We also search for the literal hex string here, so the
                    ;; search stops as soon as one is found, otherwise we too
                    ;; easily fall into the trap of bug#33708 where at every
                    ;; cycle we first search unsuccessfully through the whole
                    ;; buffer with one kind of search before trying the
                    ;; other search.
                    ;; Don't bother regexp-quoting the string since we know
                    ;; it's only made of hex chars!
                    "\\|" string)))
      (let ((case-fold-search nil))
        (funcall (if isearch-forward
                     #'re-search-forward
                   #'re-search-backward)
                 re bound noerror)))))

(defun nhexl--isearch-search-fun (orig-fun)
  (let ((def-fun (funcall orig-fun)))
    (lambda (string bound noerror)
      (unless bound
        (setq bound (if isearch-forward (point-max) (point-min))))
      ;; The order we used for the different searches is important:
      ;; - First we do the hex-address search since it's always fast even in
      ;;   very large buffers.
      ;; - Then we do the hex-bytes search.
      ;; - Only last we fallback to the def-fun: if the user wants to
      ;;   do an hex-bytes search, the def-fun will likely fail but not
      ;;   without first scanning the whole buffer which can take a while,
      ;;   as in bug#33708.
      (let ((startpos (point))
            def)
        ;; Hex address search.
        (when (and nhexl-isearch-hex-addresses
                   (> (length string) 1)
                   (string-match-p "\\`[[:xdigit:]]+:?\\'" string))
          ;; Could be a hexadecimal address.
          (goto-char startpos)
          (let ((newdef (nhexl--isearch-match-hex-address string bound noerror)))
            (when newdef
              (setq def newdef)
              (setq bound (match-beginning 0)))))
        ;; Hex bytes search
        (when (and nhexl-isearch-hex-bytes
                   (> (length string) 1)
                   (string-match-p "\\`[[:xdigit:]]+\\'" string))
          ;; Could be a search pattern specified in hex.
          (goto-char startpos)
          (let ((newdef (nhexl--isearch-match-hex-bytes string bound noerror)))
            (when newdef
              (setq def newdef)
              (setq bound (match-beginning 0)))))
        ;; Normal search.
        (progn
          (goto-char startpos)
          (let ((newdef (funcall def-fun string bound noerror)))
            (when newdef
              (setq def newdef)
              (setq bound (match-beginning 0)))))
        (when def
          (goto-char def)
          def)))))

(defun nhexl--isearch-match-hex-address (string bound _noerror)
  ;; FIXME: The code below works well to find the address, but the
  ;; resulting isearch-highlighting is wrong (the char(s) at that position
  ;; is highlighted, instead of the actual address matched in the
  ;; before-string).
  (let* ((addr (string-to-number string 16))
         ;; If `string' says "7a:", then it's "anchored", meaning that
         ;; we'll only look for nearest address of the form "XXX7a"
         ;; whereas if `string' says just "7a", then we look for nearest
         ;; address of the form "XXX7a", or "XXX7aX", or "XXX7aXX", ...
         (anchored (eq ?: (aref string (1- (length string)))))
         (mod (lsh 1 (* 4 (- (length string) (if anchored 1 0)))))
         (base (save-restriction (widen) (point-min)))
         (bestnext nil)
         (maxaddr (- (max (point) bound) base)))
    (while (< addr maxaddr)
      (let ((next (+ addr base (* (/ (- (point) base) mod) mod))))
        (if isearch-forward
            (progn
              (when (<= next (point))
                (setq next (+ next mod)))
              (cl-assert (> next (point)))
              (and (< next bound)
                   (or (null bestnext) (< next bestnext))
                   (setq bestnext next)))
          (when (>= next (point))
            (setq next (- next mod)))
          (cl-assert (< next (point)))
          (and (> next bound)
               (or (null bestnext) (> next bestnext))
               (setq bestnext next))))
      (let ((nextmod (* mod 16)))
        (if (or anchored
                ;; Overflow!  let's get out of the loop right away.
                (< nextmod mod))
            (setq maxaddr -1)
          (setq addr (* addr 16))
          (setq mod nextmod))))
    (when bestnext
      (let* ((lw (nhexl--line-width))
             (me (+ (* lw (/ (- bestnext (point-min)) lw))
                    (point-min) lw)))
        (set-match-data (list bestnext me))
        (if isearch-forward
            ;; Go to just before the last char on the line,
            ;; otherwise, the cursor ends up on the
            ;; next line!
            (1- me)
          bestnext)))))

(advice-add 'lazy-highlight-cleanup :before
            #'nhexl--isearch-highlight-cleanup)
(defun nhexl--isearch-highlight-cleanup (&rest _)
  (when (and nhexl-mode nhexl-isearch-hex-highlight)
    (with-silent-modifications
      (dolist (ol isearch-lazy-highlight-overlays)
        (when (and (overlayp ol) (eq (overlay-buffer ol) (current-buffer)))
          (put-text-property (overlay-start ol) (overlay-end ol)
                             'fontified nil))))))

(advice-add 'isearch-lazy-highlight-match :after
            #'nhexl--isearch-highlight-match)
(defun nhexl--isearch-highlight-match (&optional mb me)
  (when (and nhexl-mode nhexl-isearch-hex-highlight
             (integerp mb) (integerp me))
    (with-silent-modifications
      (put-text-property mb me 'fontified nil))))

(defun nhexl--line-width-watcher (_sym _newval op where)
  (when (eq op 'set)
    (dolist (buf (if where (list where) (buffer-list)))
      (with-current-buffer buf
        (when nhexl-mode (nhexl--flush))))))

(when (fboundp 'add-variable-watcher)
  (add-variable-watcher 'nhexl-line-width #'nhexl--line-width-watcher))

(defun nhexl--window-size-change (frame-or-window)
  (when (eq t (default-value 'nhexl-line-width))
    (if (windowp frame-or-window)         ;Emacs≥27
        (with-selected-window frame-or-window
          (nhexl--adjust-to-width))
      (dolist (win (window-list frame-or-window 'nomini))
        (when (buffer-local-value 'nhexl-mode (window-buffer win))
          (with-selected-window win (nhexl--adjust-to-width)))))))

(defun nhexl--window-config-change ()
  ;; Doing it only from `window-size-change-functions' is not sufficient
  ;; because it's not run when you set-window-buffer.
  (when (eq t (default-value 'nhexl-line-width))
    (nhexl--adjust-to-width)))
  
(defun nhexl--adjust-to-width ()
  ;; FIXME: What should we do with buffers displayed in several windows of
  ;; different width?
  (let ((win (get-buffer-window)))
    (when win
      (let* ((width (window-text-width win))
             (bytes (/ (- width
                          (eval-when-compile
                            (+ 9        ;Address
                               3        ;Spaces between address and hex area
                               4)))     ;Spaces between hex area and ascii area
                       (+ 3 (/ 1.0 nhexl-group-size)))) ;Columns per byte
             (pow2bytes (lsh 1 (truncate (log bytes 2)))))
        (when (> (/ bytes pow2bytes) 1.5)
          ;; Add 1½ steps: 4, *6*, 8, *12*, 16, *24*, 32, *48*, 64
          (setq pow2bytes (+ pow2bytes (/ pow2bytes 2))))
        (unless (eql pow2bytes nhexl-line-width)
          (setq-local nhexl-line-width pow2bytes))))))

;;;;; The main prefix command.

(defvar nhexl-universal-argument-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map universal-argument-map)
    (define-key map [?\C-u] 'universal-argument-more)
    (define-key map [remap digit-argument] 'nhexl-digit-argument)
    (dolist (k '("a" "b" "c" "d" "e" "f"))
      (define-key map k 'nhexl-digit-argument))
    map)
  "Keymap used while processing nhexl-mode's \\[universal-argument].")

;; FIXME: Using advice is ugly!

;; Instead of an advice, we'd prefer to replace universal-argument--description
;; on prefix-command-echo-keystrokes-functions, but there's no mechanism to
;; do that.
(advice-add 'universal-argument--description :around
            #'nhexl--universal-argument-description)
(defun nhexl--universal-argument-description (orig-fun &rest args)
  (cond
   ((not nhexl-mode) (apply orig-fun args))
   ((null prefix-arg) nil)
   (t
    (concat "C-u"
            (pcase prefix-arg
              (`(-) " -")
              (`(,(and (pred integerp) n))
               (let ((str ""))
                 (while (and (> n 4) (= (mod n 4) 0))
                   (setq str (concat str " C-u"))
                   (setq n (/ n 4)))
                 (if (= n 4) str (format " %s" prefix-arg))))
              ((pred integerp) (format " #x%X" prefix-arg))
              (_ (format " %s" prefix-arg)))))))

(advice-add 'universal-argument--mode :around
            #'nhexl--universal-argument-mode)
(defun nhexl--universal-argument-mode (orig-fun &rest args)
  (if (not nhexl-mode)
      (apply orig-fun args)
    (let ((universal-argument-map nhexl-universal-argument-map))
      (apply orig-fun args))))

(defun nhexl-digit-argument (arg)
  "Part of the hexadecimal numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (prefix-command-preserve-state)
  (let* ((keys (this-command-keys))
         (key (aref keys (1- (length keys))))
         (char (if (integerp key) (logand key #x7f)))
	 (digit (cond
                 ((<= ?a char ?f) (+ 10 (- char ?a)))
                 ((<= ?A char ?F) (+ 10 (- char ?A)))
                 ((<= ?0 char ?9) (- char ?0)))))
    (setq prefix-arg (cond ((integerp arg)
                            (+ (* arg 16)
			       (if (< arg 0) (- digit) digit)))
                           ((eq arg '-)
                            ;; Treat -0 as just -, so that -01 will work.
                            (if (zerop digit) '- (- digit)))
                           (t
                            digit))))
  (universal-argument--mode))

;;;; ChangeLog:

;; 2020-01-10  Alessio Di Mauro  <dimauro.alessio@gmail.com>
;; 
;; 	* packages/nhexl-mode/nhexl-mode.el (nhexl-mode-map): Remap EOL/BOL
;; 
;; 	Copyright-paperwork-exempt: yes
;; 
;; 	(nhexl-move-beginning-of-line, nhexl-move-end-of-line): New commands
;; 
;; 2019-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/nhexl-mode/nhexl-mode.el (nhexl-separate-line): New user
;; 	config
;; 
;; 	(nhexl--make-line, nhexl--jit, nhexl--header-line): Obey it.
;; 	(nhexl-mode): Better take advantage of new window-size-change-functions.
;; 
;; 2019-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/nhexl-mode/nhexl-mode.el (nhexl-group-size): New user config
;; 
;; 	(nhexl--make-line, nhexl--header-line, nhexl--adjust-to-width): Obey it.
;; 	(nhexl--window-size-change): Adjust to Emacs-27's new calling convention
;; 	of window-size-change-functions.
;; 	(nhexl-mode): Use the local part of window-size-change-functions when it
;; 	works reliably.
;; 
;; 2019-05-05  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el (nhexl-nibble-self-insert): Obey overwrite-mode
;; 
;; 	(nhexl-mode-map): Rebind overwrite-mode so we always use
;; 	binary-overwrite.
;; 	(nhexl-hex-edit-mode): New alias.
;; 
;; 2018-12-14  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode/nhexl-mode.el: Fix performance bug#33708
;; 
;; 	(nhexl--isearch-match-hex-bytes): Also search for the literal hex text. 
;; 	Make sure we use unibyte strings.
;; 	(nhexl--isearch-search-fun): Re-order the different searches.
;; 	(nhexl--isearch-highlight-cleanup, nhexl--isearch-highlight-match): 
;; 	Don't accidentally mark the buffer as modified.
;; 
;; 2018-12-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el: Add isearch and highlight to hex area
;; 
;; 	(nhexl-isearch-hex-addresses, nhexl-isearch-hex-bytes)
;; 	(nhexl-isearch-hex-highlight): New vars.
;; 	(nhexl--make-line): Copy isearch highlighting from the buffer when
;; 	applicable.
;; 	(nhexl--isearch-match-hex-bytes): New function.
;; 	(nhexl--isearch-match-hex-address): New function, extracted from 
;; 	nhexl--isearch-search-fun.  Match the whole corresponding line.
;; 	(nhexl--isearch-search-fun): Use them.
;; 	(nhexl--isearch-highlight-cleanup, nhexl--isearch-highlight-match): New
;; 	functions.
;; 	(lazy-highlight-cleanup, isearch-lazy-highlight-match): Use them as 
;; 	advice to propagate isearch highlight to the hex area.
;; 
;; 2018-11-07  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el: Improve handling of mouse events
;; 
;; 	(nhexl--posn-hexadjust): New function extracted from
;; 	nhexl-mouse-set-point.
;; 	(nhexl-mouse-set-point): Use it.
;; 	(nhexl-mouse-drag-region, nhexl-mouse-set-region): New commands.
;; 	(nhexl-mode-map): Remap to them.
;; 	(nhexl--make-line): Circumvent some misbehavior in the presence of
;; 	buttons.
;; 
;; 2018-11-06  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el: Make C-u use hexadecimal
;; 
;; 	(nhexl-universal-argument-map): New var.
;; 	(nhexl--universal-argument-description)
;; 	(nhexl--universal-argument-mode): New advice functions.
;; 	(nhexl-digit-argument): New command.
;; 
;; 2018-04-27  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode/nhexl-mode.el: Get rid of the spurious top empty line
;; 
;; 	(nhexl-mode): Set word-wrap to nil.
;; 	(nhexl-mouse-set-point): Don't bother sanity checking the string.
;; 	(nhexl--put-LF-in-string): New var.
;; 	(nhexl--make-line, nhexl--jit): Obey it.
;; 	(nhexl--header-line): Pre-construct some strings.
;; 
;; 2018-04-26  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode/nhexl-mode.el: Improve multi-window behavior
;; 
;; 	(nhexl--nibble) <var>: Remove.
;; 	(nhexl--nibbles): New var to replace it.
;; 	(nhexl--nibble) <fun>: Return the nibble offset for the selected window.
;; 	(nhexl--nibble-set): Set the nibble offset for the selected window.
;; 	(nhexl-mouse-set-point): New command.
;; 	(nhexl-mode-map): Bind it.
;; 	(nhexl--point): Move.
;; 	(nhexl--jit): Simplify back.
;; 	(nhexl--window-config-change): New function.
;; 	(nhexl-mode): Use it for window-configuration-change-hook. Immediately
;; 	adjust to width if applicable.
;; 
;; 2018-04-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode/nhexl-mode.el: Fix minor issues
;; 
;; 	Bump required Emacs to 24.4 (since we use nadvice).
;; 	(nhexl--refresh-cursor): Move.
;; 	(nhexl-overwrite-only-mode-map): Add remapping for backward-delete-char.
;; 	(nhexl--make-line): Don't refer to nhexl--point directly. Fix
;; 	highlighting of the point's address when font-lock is off.
;; 	(nhexl--jit): Pass nhexl--point to it.
;; 	(nhexl--header-line): Don't use nhexl--point so it works correctly with 
;; 	multiple windows.
;; 	(nhexl--window-config-change): Rename to nhexl--adjust-to-width.
;; 
;; 2018-04-23  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode/nhexl-mode.el (nhexl-line-width): Allow dynamic adjust
;; 
;; 	(nhexl--line-width): New function.
;; 	(nhexl--window-size-change): New function.
;; 	(nhexl-mode): Use it.
;; 	(nhexl--flush, nhexl--window-config-change): New functions.
;; 	(nhexl--jit): Set 'priority' of overlay so as not to hide the region.
;; 	(nhexl--header-line): Don't use letters past `f` for columns >15.
;; 	(nhexl--line-width-watcher): New function.
;; 	(nhexl-line-width): Use it as watcher when applicable.
;; 
;; 2018-04-19  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode/nhexl-mode.el: Let isearch look for addresses as well
;; 
;; 	(nhexl-obey-font-lock): New custom var.
;; 	(nhexl--make-line, nhexl--jit): Use it.
;; 	(nhexl-silently-convert-to-unibyte): New custom var.
;; 	(nhexl-mode): Use it.  Set isearch-search-fun-function. Don't bother
;; 	switching to unibyte for pure-ascii buffers. Be more robust for the case
;; 	when nhexl-mode is enabled while it was already enabled.
;; 	(nhexl--isearch-search-fun): New function.
;; 	(nhexl--font-lock-switch): New function.
;; 
;; 2018-04-16  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el: Hide undisplayable chars by default
;; 
;; 	(nhexl-line-width): Make it a defcustom.
;; 	(nhexl-display-unprintables): New defcustom.
;; 	(nhexl--display-table): Avoid \NNN by default.
;; 	(nhexl-mode): Suggest converting to unibyte when applicable.
;; 	(nhexl-scroll-down, nhexl-scroll-up): New commands.
;; 	(nhexl-mode-map): Use them.
;; 
;; 2018-04-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el: Bump version number for new release
;; 
;; 2018-04-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el (nhexl-overwrite-only-mode): New minor mode.
;; 
;; 2018-04-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el: Add our own line-movement functions
;; 
;; 	(nhexl-mode-map): New keymap.
;; 	(nhexl-next-line, nhexl-previous-line): New commands.
;; 	(nhexl-nibble-next-line, nhexl-nibble-previous-line): Remove.
;; 
;; 2018-04-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el (nhexl-nibble-edit-mode): New minor mode
;; 
;; 2016-08-08  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* nhexl-mode.el: Use cl-lib
;; 
;; 2012-03-25  Chong Yidong  <cyd@gnu.org>
;; 
;; 	nhexl-mode.el: Fix last change.
;; 
;; 2012-03-24  Chong Yidong  <cyd@gnu.org>
;; 
;; 	Commentary tweaks for csv-mode, ioccur, and nhexl-mode packages.
;; 
;; 2012-03-20  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add nhexl-mode.
;; 



(provide 'nhexl-mode)
;;; nhexl-mode.el ends here
