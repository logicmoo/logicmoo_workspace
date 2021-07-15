;;; lmc.el --- Little Man Computer in Elisp  -*- lexical-binding:t -*-

;; Copyright (C) 2011, 2013, 2014, 2016  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.4
;; Package-Requires ((emacs "24") (cl-lib "0.5"))

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

;; A simulator for the Little Man Computer.
;; http://en.wikipedia.org/wiki/Little_man_computer

;; The simulator uses a plain editable buffer, so you can edit the machine
;; words just like any other text, and every word can be given a name (label)
;; which can also be edited in the normal way.  Additionally to the labels it
;; shows the disassembled meaning of instruction words.  Of course, it can't
;; always know which words are meant to be code rather than data, so it relies
;; on information from the assembler to do that, and otherwise just marks every
;; word it executes as being "code".

;; The assembly uses a slightly different (Lispish) syntax where comments start
;; with ";", and each instruction needs to be wrapped in parentheses.
;; Other than that it's the same assembly as documented elsewhere
;; (accepts a few mnemonic variants, such as IN/INP, STA/STO, BR/BRA).
;; Another difference is that the DAT mnemonic accepts any number of words
;; rather than just one.
;;
;; So the assembly (stored in files with extension ".elmc") looks like:
;;
;;   label1
;;          (BR label2) ;Useless extra jump.
;;   label2
;;          (LDA data1) ;Cleverest part of the algorithm.
;;          (ADD data2)
;;          (STO data1)
;;          (BR label1)
;;          
;;   data1  (DAT 0)
;;   data2  (DAT 050 060 070)
;;
;; And actually, since the assembler re-uses the Emacs Lisp reader to parse the
;; code, you can use binary, octal, and hexadecimal constants as well, using
;; the notations #b101010, #o277, and #x5F respectively.
;; 
;; The lmc-asm-mode supports the usual editing features such as label
;; completion, mnemonic completion, jumping to a label, automatic indentation,
;; and code folding.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'hexl)

(defgroup lmc ()
  "Customization group for the Little Man Computer simulator."
  :group 'languages)

;;; The LMC-Simulator

(defvar lmc--pc 0 "Program counter for LMC.")
(make-variable-buffer-local 'lmc--pc)

(defvar lmc-acc 0 "Accumulator for LMC.")
(make-variable-buffer-local 'lmc--acc)

(defvar lmc-turbo nil
  "When non-nil, evaluate the code without extra delays.
When nil, evaluation flashes the cursor at to help you see what's going on,
which slows it down significantly.
Also, when nil, evaluation is interrupted when the user hits a key.")

;; Emacs-22 backward compatibility.
(defmacro lmc--with-silent-modifications (&rest body)
  (declare (debug t) (indent 0))
  (if (fboundp 'with-silent-modifications)
      `(with-silent-modifications ,@body)
    (let ((modified (make-symbol "modified")))
      `(let* ((,modified (buffer-modified-p))
	      (buffer-undo-list t)
	      (inhibit-read-only t)
	      (inhibit-modification-hooks t)
	      deactivate-mark
	      ;; Avoid setting and removing file locks and checking
	      ;; buffer's uptodate-ness w.r.t the underlying file.
	      buffer-file-name
	      buffer-file-truename)
	 (unwind-protect
	     (progn
	       ,@body)
	   (unless ,modified
	     (restore-buffer-modified-p nil)))))))

;; (defun lmc-check (cmds)
;;   (dolist (cmd cmds)
;;     (pcase cmd
;;       ((pred symbolp))                                ;A label.
;;       (`(,(or `IN `OUT `HLT `COB)))                   ;Arity-0 opcode.
;;       (`(,(or `LDA `STO `ADD `SUB `BR `BRZ `BRP `DAT) ;Arity-1 opcode.
;;          ,(or (pred lmc--numberp) (pred symbolp))))
;;       (_ (error "Unknown instruction %S" cmd)))))

(defun lmc--numberp (n max)
  (when (numberp n)
    (or (and (or (natnump n) (error "%S is not a positive integer" n))
             (or (< n max) (error "%S is too large" n))))))

(defun lmc--resolve (arg labels max)
  (if (lmc--numberp arg max) arg
    (or (cdr (assq arg labels))
        (error (if (symbolp arg)
                   "Unknown label %S"
                 "Arg %S is neither a label nor a number")
               arg))))

(defconst lmc-mnemonic-1-table '((LDA . 5)
                                 (STO . 3) (STA . 3)
                                 (ADD . 1)
                                 (SUB . 2)
                                 (BR . 6) (BRA . 6)
                                 (BRZ . 7)
                                 (BRP . 8))
  "Mnemonic table for arity-1 instructions.")

(defconst lmc-mnemonic-0-table '((HLT . 000) (COB . 000)
                                 (IN . 901) (INP . 901)
                                 (OUT . 902))
  "Mnemonic table for arity-0 instructions.")

(defun lmc--assemble (cmds)
  ;; FIXME: Move to error position upon error.
  (let ((pos 0)
        (labels ()))
    ;; First pass, resolve labels to their positions.
    (dolist (cmd cmds)
      (setq cmd (cdr cmd))              ;Ignore position info at this stage.
      (cond
       ((or (consp cmd)
            (assq cmd lmc-mnemonic-0-table))
        (setq pos (+ pos (if (eq (car cmd) 'DAT)
                             (1- (length cmd)) 1))))
       ((numberp cmd)
        (cond
         ((not (and (natnump cmd) (< cmd 100)))
          (error "%S is not a valid address" cmd))
         ((< cmd pos)
          (error "Address %S already used" cmd))
         ((rassq pos labels)
          (error "Label %S needs to come after address %S"
                 (car (rassq pos labels)) cmd))
         (t (setq pos cmd))))
       ((and cmd (symbolp cmd))
        ;; (cl-assert (symbolp cmd))
        (if (assq cmd labels)
            (error "Duplicate label %S" cmd)
          (push (cons cmd pos) labels)))))
    ;; Second pass, do the actual assembly.
    (let* ((words ())
           (ll nil)
           (newword
            (lambda (w &optional code)
              (push (list w ll code) words) (setq ll nil))))
      (dolist (cmd cmds)
        (goto-char (pop cmd))          ;Move to start of CMD, in case of error.
        (cond
         ((assq cmd lmc-mnemonic-0-table)
          (funcall newword (cdr (assq cmd lmc-mnemonic-0-table)) 'code))
         ((and (null (cdr-safe cmd))
               (assq (car-safe cmd) lmc-mnemonic-0-table))
          (funcall newword (cdr (assq (car cmd) lmc-mnemonic-0-table)) 'code))
         ((eq (car-safe cmd) 'DAT)
          (dolist (arg (cdr cmd))
            (funcall newword (lmc--resolve arg labels 1000))))
         ((assq (car-safe cmd) lmc-mnemonic-1-table)
          (funcall newword
                   (+ (* 100 (cdr (assq (car cmd) lmc-mnemonic-1-table)))
                      (lmc--resolve (nth 1 cmd) labels 100))
                   'code))
         ((numberp cmd)
          (dotimes (_ (- cmd (length words)))
            (funcall newword 0)))
         ((and cmd (symbolp cmd))
          (cl-assert (eq (cdr (assq cmd labels)) (length words)))
          (setq ll cmd))
         (t (error "Invalid instruction %S" cmd))))
      (nreverse words))))

;; (defvar lmc-label-width 8)

(defun lmc--load-word (word addr)
  (cl-assert (bolp))
  (insert (propertize (format " %02d:\t" addr)
                      'read-only t
                      'front-sticky t
                      'rear-nonsticky t))
  (let ((word (car word))
        (label (nth 1 word))
        (code (nth 2 word)))
    (let () ;; ((basepos (point)) (base (current-column)))
      (if (and label (symbolp label))
          (insert (symbol-name label)))
      ;; (when (>= (current-column) (+ base tab-width))
      ;;   (while (>= (current-column) (+ base tab-width -1))
      ;;     (delete-char -1))
      ;;   (insert "…")
      ;;   (put-text-property basepos (point)
      ;;                      'help-echo (symbol-name label)))
      ;; (insert (propertize
      ;;      (make-string (1+ (- lmc-label-width (current-column))) ?\s)
      ;;      'display '(space :align-to (1+ lmc-label-width))))
      (insert (eval-when-compile (propertize "\t"
                                             'read-only t
                                             'rear-nonsticky t))))
    (insert (format "  %03d" word))
    (insert (if code
                (eval-when-compile (propertize "\n"
                                               'lmc-code t
                                               'read-only t
                                               'rear-nonsticky t))
              (eval-when-compile (propertize "\n"
                                             'read-only t
                                             'rear-nonsticky t))))))

(defun lmc-disassemble-word (word)
  (let ((code (car (rassq (/ word 100) lmc-mnemonic-1-table))))
    (cond
     (code (list code (mod word 100)))
     ((rassq word lmc-mnemonic-0-table)
      (list (car (rassq word lmc-mnemonic-0-table)))))))

(defun lmc-addr->point (addr)
  (goto-char (point-min))
  (forward-line addr))

(defun lmc-point->addr ()
  (- (count-lines (point-min) (point)) (if (bolp) 0 1)))

(defun lmc-get-word (&optional addr fix)
  (save-excursion
    (if (null addr)
        (forward-line 0)
      (lmc-addr->point addr))
    (cond
     ((re-search-forward "\t.*\t  \\([0-9][0-9][0-9]\\)$"
                         (line-end-position) t)
      (string-to-number (match-string 1)))
     ((re-search-forward "\t.*\t\\(.*\\)$" (line-end-position) t)
      (let ((n (string-to-number (match-string 1))))
        (unless (integerp n) (setq n (truncate n)))
        (setq n (mod n 1000))
        (when fix
          (replace-match (format "  %03d" n) t t nil 1))
        n))
     (t 0))))

(defconst lmc-label-re "^\\([^\t\n]*\\)\t\\(.*\\)\t *[0-9]")

(defvar lmc-label-table nil)

(defun lmc-record-label (addr label)
  (let ((old (aref lmc-label-table addr)))
    (unless (and old (equal (car old) label))
      ;; (message "recordlabel %S = %S" addr label)
      (aset lmc-label-table addr (list label))
      (when (cdr old)
        (run-with-timer
         0 nil
         (lambda (buf refaddrs)
           (with-current-buffer buf
             (save-excursion
               ;; (message "refreshlabel in %S" refaddrs)
               (dolist (refaddr refaddrs)
                 (lmc-addr->point (1+ refaddr))
                 (unless (bobp)
                   (let ((inhibit-read-only t))
                     (put-text-property (1- (point)) (point)
                                        'fontified nil)))))))
         (current-buffer) (cdr old))))))

(defun lmc-get-label (addr)
  (save-excursion
    ;; (if (null addr)
    ;;     (forward-line 0)
    (lmc-addr->point addr) ;; )
    (let ((label (when (re-search-forward lmc-label-re nil t)
                   (if (> (match-end 2) (match-beginning 2))
                       (match-string 2)))))
      (lmc-record-label addr label)
      label)))


(defun lmc-font-lock-opcode ()
  (save-match-data
    (when (get-text-property (line-end-position) 'lmc-code)
      (let* ((word (lmc-get-word))
             (code (lmc-disassemble-word word)))
        ;; Resolve labels.
        (when (integerp (nth 1 code))
          (let* ((addr (nth 1 code))
                 (label (lmc-get-label addr)))
            (cl-pushnew (lmc-point->addr)
                        (cdr (aref lmc-label-table addr)))
            (when label
              (setf (nth 1 code) label))))
        (put-text-property
         (line-end-position) (1+ (line-end-position))
         'display
         (format (eval-when-compile
                   (concat (propertize "\t" 'cursor t)
                           (propertize "%s" 'face font-lock-comment-face)
                           "\n"))
                 (or code '(Invalid opcode)))))
      nil)))

(defun lmc-font-lock-label ()
  (lmc-record-label (lmc-point->addr)
                    (if (> (match-end 2) (match-beginning 2))
                       (match-string 2)))
  (save-excursion
    ;; ;; Replace any TAB found in label.
    ;; (goto-char (match-beginning 2))
    ;; (while (progn (skip-chars-forward "^\t" (match-end 2))
    ;;               (< (point) (match-end 2)))
    ;;   (insert " ") (delete-char 1))
    ;; Truncate label's display if needed.
    (move-to-column (1- (* 2 tab-width)))
    (when (> (match-end 2) (point))
      (forward-char -1)
      (put-text-property (match-beginning 2) (match-end 2)
                         'help-echo (match-string 2))
      (put-text-property (point) (match-end 2) 'display "…")))
  font-lock-constant-face)

(defconst lmc-font-lock-keywords
  `((,lmc-label-re
     (1 'hexl-address-region)
     (2 (lmc-font-lock-label)))
    (".$" (0 (lmc-font-lock-opcode)))))

(defun lmc-after-change (beg end _len)
  (unless inhibit-read-only
    (save-excursion
      ;; Replace any TAB or NL inserted, which could interfere with parsing.
      (goto-char beg)
      (while (progn (skip-chars-forward "^\t\n" end)
                    (< (point) end))
        (insert " ") (delete-char 1)))))

(defvar lmc-pc 0 "LMC program counter.")
(make-variable-buffer-local 'lmc-pc)
(defvar lmc-acc nil "LMC accumulator.")
(make-variable-buffer-local 'lmc-acc)
(defvar lmc-output nil "Past LMC output.")
(make-variable-buffer-local 'lmc-output)

(defvar lmc--stopped nil "State where we stopped.")
(make-variable-buffer-local 'lmc--stopped)

(defun lmc-update-pc ()
  (setq lmc-pc (mod lmc-pc 100))
  (lmc-addr->point lmc-pc)
  (move-marker overlay-arrow-position (point))
  (re-search-forward "\t.*\t *" nil t)
  (unless (get-text-property (line-end-position) 'lmc-code)
    (let ((inhibit-read-only t))
      (put-text-property (line-end-position)
                         (min (1+ (line-end-position)) (point-max))
                         'lmc-code t))))

(defun lmc--state ()
  (list (buffer-chars-modified-tick) lmc-acc lmc-pc))
(defun lmc-stopped-p ()
  (equal (lmc--state) lmc--stopped))

;; FIXME: Add tool-bar to LMC-Sim.

(defvar lmc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" 'lmc-step)
    (define-key map "\C-c\C-r" 'lmc-run)
    (define-key map "\C-c\C-l" 'lmc-load-file)
    (define-key map "\C-c\C-a" 'lmc-set-acc)
    (define-key map "\C-c\C-p" 'lmc-set-pc)
    map))

(easy-menu-define lmc-menu lmc-mode-map "Menu for LMC-Sim."
  '("LMC-Sim"
    ["Step" lmc-step (not (lmc-stopped-p))]
    ["Run" lmc-run (not (lmc-stopped-p))]
    ["Load file" lmc-load-file]
    "--"
    ["Set Program Counter" lmc-set-pc]
    ["Set Accumulator" lmc-set-acc]))

(defvar lmc-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "gud/next" 'lmc-step 'step map
                         :label "Step" ;; :vert-only t
                         :enable '(not (lmc-stopped-p))
                         )
    (tool-bar-local-item "gud/run" 'lmc-run 'run map
                         :label "Run" ;; :vert-only t
                         :enable '(not (lmc-stopped-p))
                         )
    map))

(defun lmc-tool-bar-to-string (&optional map)
  (let ((res ""))
    (map-keymap
     (lambda (_k v)
       (when (eq (car v) 'menu-item)
         (let* ((label (nth 1 v))
                (cmd (nth 2 v))
                (plist (nthcdr (if (consp (nth 3 v)) 4 3) v))
                (help-echo (plist-get plist :help))
                (image     (plist-get plist :image))
                (enable-exp (if (plist-member plist :enable)
                                (plist-get plist :enable)
                              t))
                (enable (eval enable-exp))
                (map (let ((map (make-sparse-keymap)))
                       (define-key map [header-line mouse-1] cmd)
                       (define-key map [header-line mouse-2] cmd)
                       map))
                (button
                 (propertize " " 'help-echo (or help-echo label)
                             'keymap map
                             'face 'header-line
                             'mouse-face (if enable 'mode-line-highlight)
                             'rear-nonsticky '(display keymap help-echo)
                             'display (if (and (eq 'image (car image))
                                               (not enable))
                                          `(image :conversion disabled
                                                  ,@(cdr image))
                                        image))))
           (setq res (concat res (propertize " " 'display '(space :width 0.5)
                                             'face 'header-line
                                             )
                             button)))))
     (or (let ((tool-bar-map map)) (tool-bar-make-keymap))
         (key-binding [tool-bar])))
    res))

(define-derived-mode lmc-mode fundamental-mode "LMC-Sim"
  "The simulator of the Little Man Computer."
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'truncate-partial-width-windows) t)
  (set (make-local-variable 'tab-width) 10)
  (set (make-local-variable 'font-lock-defaults)
       '(lmc-font-lock-keywords t))
  (set (make-local-variable 'font-lock-extra-managed-props)
       '(display help-echo))
  ;; (set (make-local-variable 'tool-bar-map) lmc-tool-bar-map)
  (add-hook 'after-change-functions #'lmc-after-change nil t)
  (set (make-local-variable 'lmc-label-table) (make-vector 100 nil))
  (set (make-local-variable 'overlay-arrow-position) (point-min-marker))
  (lmc-update-pc)
  ;; (overwrite-mode 1)
  (set (make-local-variable 'header-line-format)
       `(""
         (:eval (lmc-tool-bar-to-string lmc-tool-bar-map))
         "  " ,(propertize "LMC-Sim" 'face '(bold italic)) "  "
         ,(propertize "PC=" 'face 'font-lock-function-name-face)
         (:eval (format ,(propertize "%02d"
                                     'mouse-face 'mode-line-highlight
                                     'help-echo
                                     "mouse-2: set the Program Counter"
                                     'follow-link t
                                     ;; I'm having problems with mouse-2 to
                                     ;; mouse-1 remapping in the mode-line and
                                     ;; header-line, so I over-do it a bit.
                                     'keymap
                                     '(keymap
                                       (header-line keymap
                                                    (down-mouse-1 . ignore)
                                                    (mouse-2 . lmc-set-pc)
                                                    (mouse-1 . lmc-set-pc))))
                        lmc-pc))
         "  " ,(propertize "ACC=" 'face 'font-lock-function-name-face)
         (:eval (format ,(propertize "%03d"
                                     'mouse-face 'mode-line-highlight
                                     'help-echo "mouse-2: set the Accumulator"
                                     'follow-link t
                                     'keymap
                                     ;; I'm having problems with mouse-2 to
                                     ;; mouse-1 remapping in the mode-line and
                                     ;; header-line, so I over-do it a bit.
                                     '(keymap
                                       (header-line keymap
                                                    (down-mouse-1 . ignore)
                                                    (mouse-2 . lmc-set-acc)
                                                    (mouse-1 . lmc-set-acc))))
                        lmc-acc))
         "      " ,(propertize "Recent output="
                               'face 'font-lock-function-name-face)
         (:eval (if lmc-output (format "%s" lmc-output) "()"))))
  )

(defun lmc-set-pc (pc)
  "Set the Program Counter."
  (interactive (list (read-number "New PC: " lmc-pc)))
  (setq lmc-pc pc)
  (lmc-update-pc))

(defun lmc-set-acc (acc)
  "Set the Accumulator."
  (interactive (list (read-number "New Accumulator: " lmc-acc)))
  (setq lmc-acc (mod acc 1000)))

(defun lmc-load (words)
  (pop-to-buffer "*LMC-Sim*")
  (lmc-mode)
  (let ((inhibit-read-only t)
        (addr 0))
    (setq lmc-pc 0)
    (setq lmc-acc 0)
    (setq lmc-output nil)
    (erase-buffer)
    (dolist (word words)
      (lmc--load-word word addr)
      (setq addr (1+ addr)))
    (while (< addr 100)
      (lmc--load-word '(0) addr)
      (setq addr (1+ addr))))
  (lmc-update-pc))

(defcustom lmc-store-flash t
  "If non-nil, memory words blink when modified."
  :type 'boolean)

(defun lmc--sit-for (secs)
  (unless lmc-turbo (sit-for secs)))

(defun lmc-store-word (addr word)
  (save-excursion
    (lmc-addr->point addr)
    (if (not (re-search-forward "\t.*\t\\(.*\\)$" (line-end-position) t))
        (error "Missing memory cell %S" addr)
      (let ((mb1 (match-beginning 1)))
        (when lmc-store-flash
          (lmc--with-silent-modifications
           (put-text-property mb1 (point) 'face 'region))
          (lmc--sit-for 0.2))
        (let ((me1 (point)))
          (insert (format "  %03d" word)) (delete-region mb1 me1))
        (when lmc-store-flash
          (lmc--sit-for 0.1)
          (lmc--with-silent-modifications
           (put-text-property mb1 (point) 'face 'region))
          (lmc--sit-for 0.1)
          (lmc--with-silent-modifications
           (put-text-property mb1 (point) 'face nil))
          (lmc--sit-for 0.1))))))

(defun lmc-step ()
  "Execute one LMC instruction."
  (interactive)
  (let* ((inst (lmc-get-word lmc-pc 'fix))
         (code (lmc-disassemble-word inst)))
    (pcase (car code)
      (`HLT (if (lmc-stopped-p)
               (error "Already halted")
             (setq lmc--stopped (lmc--state))
             (force-mode-line-update)
             (message "Done.")))
      (`IN (setq lmc-acc (mod (read-number "Enter a number: ") 1000))
          (cl-incf lmc-pc))
      (`OUT (message "Output: %03d" lmc-acc)
           (push (format "%03d" lmc-acc) lmc-output)
           (cl-incf lmc-pc))
      (`LDA (setq lmc-acc (lmc-get-word (nth 1 code)))
           (cl-incf lmc-pc))
      (`STO (lmc-store-word (nth 1 code) lmc-acc)
           (cl-incf lmc-pc))
      (`ADD (setq lmc-acc (mod (+ lmc-acc (lmc-get-word (nth 1 code)))
                              1000))
           (cl-incf lmc-pc))
      (`SUB (setq lmc-acc (mod (- lmc-acc (lmc-get-word (nth 1 code)))
                              1000))
           (cl-incf lmc-pc))
      (`BR (setq lmc-pc (nth 1 code)))
      (`BRZ (setq lmc-pc (if (zerop lmc-acc)
                            (nth 1 code)
                          (1+ lmc-pc))))
      (`BRP (setq lmc-pc (if (< lmc-acc 500)
                            (nth 1 code)
                          (1+ lmc-pc))))
      (`nil (error "Invalid instruction %S" inst))
      (_ (error "%S not implemented" code))))
  (lmc-update-pc))

(defun lmc-run ()
  "Run the code until hitting a HLT.
The machine will also stop if the user presses a key."
  (interactive)
  (while (not (or (unless lmc-turbo (input-pending-p)) (lmc-stopped-p)))
    (lmc-step)
    (lmc--sit-for 0.05)))

;;; The LMC assembly language editor.

(defvar lmc-asm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; FIXME: Add "load" and "assemble" buttons.
    (define-key map "\C-c\C-l" 'lmc-asm-load)
    (define-key map "\C-c\C-a" 'lmc-asm-assemble)
    map))

(easy-menu-define lmc-asm-menu lmc-asm-mode-map
  "Menu for the LMC-Asm mode."
  '("LMC-Asm"
    ["Assemble" lmc-asm-assemble]
    ["Load into Simulator" lmc-asm-load]))


(defconst lmc-asm-mnemonic-names
  (mapcar #'symbol-name
          (append (mapcar #'car lmc-mnemonic-1-table)
                  (mapcar #'car lmc-mnemonic-0-table)
                  '(DAT))))

(defconst lmc-asm-mnemonic-names-re (regexp-opt lmc-asm-mnemonic-names))

(defvar lmc-asm-font-lock-keywords
  `(("^[ \t]*\\(?:\\sw\\|\\s_\\)+"
     (0 (if (zerop (nth 0 (syntax-ppss))) font-lock-constant-face)))
    (,(concat "(\\(" lmc-asm-mnemonic-names-re "\\_>\\)")
     (1 font-lock-keyword-face))))

(defvar lmc-asm-imenu-generic-expression
  '((nil "^\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)))

(defvar lmc-asm-outline-regexp "^\\(?:\\sw\\|\\s_\\)")

;; We use the ".elmc" extension since the syntax is not identical to
;; the usual ".lmc" syntax.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elmc\\'" . lmc-asm-mode))

;;;###autoload
(define-derived-mode lmc-asm-mode fundamental-mode "LMC-Asm"
  "Major mode to edit LMC assembly code."
  :syntax-table emacs-lisp-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(lmc-asm-font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       #'lmc-asm-indent-line)
  (set (make-local-variable 'indent-tabs-mode) t)
  (set (make-local-variable 'imenu-generic-expression)
       lmc-asm-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) lmc-asm-outline-regexp)
  (add-hook 'completion-at-point-functions #'lmc-asm-completion nil t)
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  )

(defun lmc-asm-labels (string)
  (save-excursion
    ;; We don't want to count the label being completed as a completion
    ;; candidate, so let's keep track of the original position of point and
    ;; skip any label nearby.
    (let ((point (point)))
      (goto-char (point-min))
      (let ((ls ())
            (re (concat "\\(^\\|(" lmc-asm-mnemonic-names-re "[ \t]+" "\\)"
                        (regexp-quote string) "\\(?:\\sw\\|\\s_\\)"
                        (if (> (length string) 0) "*" "+"))))
        (while (re-search-forward re nil t)
          (when (or (< point (match-end 1))
                    (> (match-beginning 1) point))
            (push (buffer-substring-no-properties
                   (match-end 1) (match-end 0)) ls)))
        ls))))

(defun lmc-asm-completion ()
  (save-excursion
    (let ((ppss (syntax-ppss)))
      (cond
       ((nth 8 ppss) nil)               ;Inside string or comment.
       ((zerop (nth 0 ppss))
        (skip-syntax-backward "w_")
        (when (save-excursion (skip-chars-backward " \t") (bolp))
          (list (point)
                (save-excursion (skip-syntax-forward "w_") (point))
                (completion-table-dynamic #'lmc-asm-labels))))
       ((= 1 (nth 0 ppss))              ;Inside paren.
        (skip-syntax-backward "w_")
        (list (point)
              (save-excursion (skip-syntax-forward "w_") (point))
              (if (eq (char-before) ?\()
                  lmc-asm-mnemonic-names
                (completion-table-dynamic #'lmc-asm-labels))))))))

(defun lmc-asm-indentation ()
  (save-excursion
    (back-to-indentation)
    (cond
     ((> (nth 0 (syntax-ppss)) 0) nil)
     ((looking-at "(") tab-width)
     ((not (looking-at comment-start-skip))
      (if (looking-at "[ \t]*$") tab-width 0))
     ((not (looking-at "\\s<\\s<")) nil)
     ((save-excursion (forward-comment (- (point))) (bobp)) 0)
     (t (forward-comment (point-max)) (lmc-asm-indentation)))))

(defun lmc-asm-indent-line (&optional arg)
  (save-excursion
    (back-to-indentation)
    (when (and (zerop (nth 0 (syntax-ppss)))
               (looking-at (concat lmc-asm-mnemonic-names-re "\\_>")))
      ;; Apparently the user forgot to parenthesize the instruction.
      (insert "(")
      (if (assq (read (current-buffer)) lmc-mnemonic-0-table)
          (insert ")")
        (let ((eol (line-end-position)))
          (ignore-errors
            (read (current-buffer))
            (when (<= (point) eol)
              (insert ")")))))))
  (let ((indent (lmc-asm-indentation)))
    (cond
     ((null indent) (lisp-indent-line arg))
     (t
      (let ((left-margin indent)) (indent-to-left-margin))
      (when (zerop indent)
        ;; Indent code (if any) after a label.
        (save-excursion
          (beginning-of-line)
          (when (looking-at "\\(?:\\sw\\|\\s_\\)+\\([ \t]*\\)(")
            (goto-char (match-beginning 1))
            (if (< (current-column) tab-width)
                (unless (save-excursion
                          (goto-char (match-end 1))
                          (= (current-column) tab-width))
                  (delete-region (match-beginning 1) (match-end 1))
                  (indent-to tab-width))
              (unless (equal (match-string 1) " ")
                (delete-region (match-beginning 1) (match-end 1))
                (insert " "))))))))))

(defun lmc-asm-read ()
  (let ((prog ())
        (initialpos (point)))
    (goto-char (point-min))
    (while (progn (forward-comment (point-max))
                  (not (eobp)))
      (let ((start (point)))
        (condition-case nil
            (push (cons (point) (read (current-buffer))) prog)
          (end-of-file (goto-char start) (signal 'end-of-file nil)))))
    (goto-char initialpos)
    (nreverse prog)))

(defun lmc-asm-load ()
  "Load current buffer into the LMC simulator."
  (interactive)
  (let ((initialpos (point))
        (window (if (eq (current-buffer) (window-buffer)) (selected-window))))
    (save-current-buffer
      (lmc-load (lmc--assemble (lmc-asm-read))))
    (goto-char initialpos)
    (if (and window (eq (current-buffer) (window-buffer window)))
        (set-window-point window (point)))))

(defun lmc-asm-assemble ()
  "Assemble current buffer to check syntax."
  (interactive)
  (let ((initialpos (point)))
    (lmc--assemble (lmc-asm-read))
    (goto-char initialpos)
    (message "No errors found")))

(defun lmc-load-file (file)
  "Load FILE into the LMC simulator."
  (interactive
   (list (read-file-name "Load LMC file: " nil nil t nil
                         (lambda (file)
                           (or (file-directory-p file)
                               (string-match-p "\\.elmc\\'" file))))))
  (let ((exists (find-buffer-visiting file))
        (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (condition-case err
              (lmc-asm-load)
            (error (error "Error at line %d: %s" (line-number-at-pos)
                          (error-message-string err)))))
      (unless exists (kill-buffer buf)))))

;;;; ChangeLog:

;; 2016-08-08  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* lmc.el: Mention dependencies & bumpp version
;; 
;; 2016-08-08  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* lmc.el: Use cl-lib
;; 
;; 2014-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/lmc/lmc.el (lmc--load-word): Remove unused vars.
;; 	(lmc-after-change, lmc-tool-bar-to-string): Mark unused args.
;; 
;; 2014-05-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add "rudel" to the list of externals.
;; 
;; 2014-01-28  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* lmc.el (lmc-asm-indentation): Indent to tab-width by default.
;; 	(lmc-step): Fix prompt for IN.
;; 
;; 2013-04-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* lmc.el (lmc--sit-for): Fix last change.
;; 
;; 2013-04-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* lmc.el (lmc-turbo): New option.
;; 	(lmc--sit-for): New function.  Use it instead of sit-for.
;; 	(lmc-run): Don't stop upon user input if lmc-turbo is enabled.
;; 
;; 2013-03-20  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/lmc/lmc.el (lmc-store-word): match-data can change in
;; 	sit-for. Reported by William Léchelle <william.lechelle@umontreal.ca>.
;; 
;; 2011-10-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* lmc.el: Make it work on Emacs-22.
;; 	(lmc--with-silent-modifications): Compatibility macro.
;; 	(lmc-store-word): Use it.
;; 	(lmc): New group.
;; 	(lmc-store-flash): Make into a defcustom.
;; 
;; 2011-09-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	2011-09-29  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* lmc.el: Version 1.0.
;; 	(lmc--assemble): Understand numerical "labels".
;; 	(lmc-tool-bar-to-string): New function.
;; 	(lmc-mode): Use it to move the buttons from the tool-bar to the
;; 	header-line.
;; 
;; 2011-09-27  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* lmc.el: Add a few more commands, and a tool-bar.
;; 	(lmc--assemble): Warn of duplicate labels.
;; 	(lmc-disassemble-word): Fix typo.
;; 	(lmc-mode-map, lmc-menu): Add bindings for run, set-pc, and set-acc.
;; 	(lmc-tool-bar-map): New var.
;; 	(lmc-mode); Use it.  Make the PC and Acc editable with a click.
;; 	(lmc-set-pc, lmc-set-acc, lmc-run): New commands.
;; 	(lmc-step): Update tool-bar after hitting HLT.
;; 	(lmc-asm-indent-line): Parenthesize if needed.
;; 
;; 2011-09-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	New package lmc.
;; 


(provide 'lmc)
;;; lmc.el ends here
