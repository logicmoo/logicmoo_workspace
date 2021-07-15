;;; fum.el --- 

;; Copyright (C)  1995, 2017 Free Software Foundation, Inc.

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Version: $Id: fum.el,v 1.1 2000/04/12 16:44:26 lenst Exp $
;; Keywords: 
;; Last edited: Sat Aug 31 23:35:29 1996 by lenst@triton.lstaflin.pp.se (Lennart Staflin)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; LCD Archive Entry:
;; fum|Lennart Staflin|lenst@lysator.liu.se|
;; |
;; 21-May-95|$Revision: 1.1 $||

;;; Commentary:

;; 

;;; Code:

;;;; Translation macros

(defun macroexpand-next ()
  (interactive)
  (let ((form (car (read-from-string (thing-at-point 'sexp)))))
    (switch-to-buffer "*MACRO*")
    (erase-buffer)
    (cl-prettyexpand form)))

(define-key emacs-lisp-mode-map "\C-cm" 'macroexpand-next)

(defun replace-grammar ()
  (interactive)
  (switch-to-buffer "fum.el")
  (goto-char (point-max))
  (backward-page 1)
  (let ((end (point)))
    (backward-page 1)
    (kill-region (point) (1- end))
    (insert-buffer "*Formatted*")))

(defun spt-nt-name (id)
  (intern (format "sgml-parse-nt-%s" id)))

(defvar *current-id* nil)

(defmacro defnt (id synexp)
  (setq *current-id* id)
  `(defun ,(spt-nt-name id) (check)
     (message "Enter %s" ,id)
     (let ((res 
	    ,(spt-synexp 'check synexp)))
       (message "Exit %s: %s" ,id res)
       res)))

(defun spt-synexp (check synexp)
  "Translate the syntax expression SYNEX to lisp with check option CHECK.
The check option can be `t', `nil', or a variable name."
  (cond ((stringp synexp)		; Token
	 (spt-token check synexp))
	((consp synexp)
	 (case (car synexp)
	   ((delim)
	    (spt-delim check (cadr synexp)))
	   ((nt)
	    (spt-nt check (cadr synexp)))
	   ((seq)
	    (spt-seq check (cdr synexp)))
	   ((alt)
	    (spt-alt check (cdr synexp)))
	   ((must once)
	    (spt-synexp check (cadr synexp)))
	   ((many)
	    (spt-many check (cadr synexp)))
	   ((opt)
	    (spt-opt check (cadr synexp)))
	   ((var)
	    (spt-var check (cadr synexp) (caddr synexp)))
	   (else
	    (error "Illegal syntax expression: %s" synexp))))))


(defun spt-var (check var synexp)
  (if (null var)
      (spt-synexp check synexp)
    `(let ((result ,(spt-synexp check synexp)))
       (message "%s: %s" ',var result) (sit-for 1)
       result)))

(defun spt-delim (check delim)
  ;; FIXME context constraint
  (let ((err-form `(sgml-delimiter-parse-error ,delim)))
    `(or (sgml-parse-delim ,delim)
	 ,(cond ((eq check t)
		 err-form)
		((eq check nil) nil)
		(t
		 `(if ,check ,err-form))))))

(defun spt-token (check token)
  `(sp-the-token ,check ,(sgml-general-case token)))


(defun spt-seq (check synexps)
  (let ((main `(and ,@(mapcar #'(lambda (synexp)
				  (spt-synexp check synexp))
			      synexps))))
    (if (or (eq check t)
	    (null (cdr synexps)))	; no need to restore point if only one
	main
      `(let ((start (point)))
	 (or ,main
	     (progn (goto-char start)
		    nil))))))

(defun spt-alt (check synexps)
  (let* ((rev (reverse synexps))
	 (last (spt-synexp check (car rev)))
	 (rest (mapcar #'(lambda (synexp) (spt-synexp nil synexp))
		       (cdr rev))))
    `(or ,@(nreverse rest)
	 ,last)))

(defun spt-many (check synexp)
  (cond
   ;; optimize special cases
   ((equal synexp '(seq (nt "65")))
    (cond ((eq check nil)
	   '(sgml-parse-many-ps))
	  (t
	   `(sgml-parse-many-must-ps ,check))))

   ;; Normal case
   (t
    `(let ((res ,(spt-synexp check synexp)))
       (and res
	    (while ,(spt-synexp nil synexp)))
       res))))

(defun spt-opt (check synexp)
  `(or ,(spt-synexp nil synexp)
       t))

(defun spt-nt (check id)
  (when (equal id "65")
    (message "OOPS: 65 is left in def: %s" *current-id*))
  (list (spt-nt-name id) check))


;;;; Help functions

(defun sp-the-token (check token)
  (let* ((start (point))
	 (next-token (sgml-parse-nametoken)))
    (if (and next-token (equal next-token token))
	token
      (if check
	  (sgml-parse-error "Expected %s" token)
	(goto-char start)
	nil))))

(defun sgml-parse-many-ps ()
  (sgml-skip-ps)
  t)

(defun sgml-parse-many-must-ps (check)
  (let ((here (point)))
    (sgml-skip-ps)
    (if (= here (point))
	(progn (if check (sgml-parse-error "Expecting parameter separator"))
	       nil)
      t)))


(defun sgml-parse-nt-55 (check)
  ;; name
  (if check (sgml-parse-name) (sgml-check-name)))


(defun sgml-parse-nt-56 (check)
  ;; number
  (let ((start (point))
	(tok (sgml-parse-nametoken)))
    (if (and tok (string-match "^[0-9]+$" tok))
	tok
	(progn
	  (goto-char start)
	  (if check
	      (sgml-parse-error "Expected a number"))
	  nil))))

(defalias 'sgml-parse-nt-64 ;; character number
  'sgml-parse-nt-56)

(defun sgml-parse-nt-76 (check)
  ;; minimum literal
  (if check
      (sgml-check-minimum-literal)
    (sgml-parse-minimum-literal)))

(defalias 'sgml-parse-nt-74 ;; public identifier
  'sgml-parse-nt-76)

(defun sgml-parse-nt-66 (check)
  ;; parameter literal
  (if check
      (sgml-check-literal)
    (sgml-parse-literal)))


;;;; SGML Declaration Grammar


(defnt "171+"
  ;; SGML declaration
  (var nil (must (once (seq
    (delim "MDO") "SGML"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "76")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "172")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "180")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "181")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "182")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "195")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "199")
    (var nil (opt (many (seq
      (nt "65") ))))
    (delim "MDC") ))))
  )


(defnt "181"
  ;; concrete syntax scope
  (var nil (must (once (seq
    "SCOPE"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "DOCUMENT" "INSTANCE" ))))
    ))))
  )


(defnt "172"
  ;; document character set
  (var nil (must (once (seq
    "CHARSET"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "173+") ))))
  )


(defnt "173+"
  ;; character set description
  (var nil (must (once (seq
    (nt "174")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "175")
    (var nil (opt (many (seq
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "174")
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "175") ))))
    ))))
  )


(defnt "174"
  ;; base character set
  (var nil (must (once (seq
    "BASESET"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "74") ))))
  )


(defnt "175"
  ;; described character set portion
  (var nil (must (once (seq
    "DESCSET"
    (var nil (must (many (seq
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "176") ))))
    ))))
  )


(defnt "176"
  ;; character description
  (var nil (must (once (seq
    (nt "177")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "179")
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      (nt "178") (nt "76") "UNUSED" ))))
    ))))
  )


(defnt "177"
  ;; described set character number
  (var nil (must (once (seq
    (nt "64") ))))
  )


(defnt "178"
  ;; base set character number
  (var nil (must (once (seq
    (nt "64") ))))
  )


(defnt "179"
  ;; number of characters
  (var nil (must (once (seq
    (nt "56") ))))
  )


(defnt "180"
  ;; capacity set
  (var nil (must (once (seq
    "CAPACITY"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      (var nil (must (once (seq
        "PUBLIC"
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "74") ))))
      (var nil (must (once (seq
        "SGMLREF"
        (var nil (must (many (seq
          (var nil (must (many (seq
            (nt "65") ))))
          (nt "55")
          (var nil (must (many (seq
            (nt "65") ))))
          (nt "56") ))))
        ))))
      ))))
    ))))
  )


(defnt "182"
  ;; concrete syntax
  (var nil (must (once (seq
    "SYNTAX"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      (nt "183")
      (var nil (must (once (seq
        (nt "184")
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "185")
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "186")
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "189")
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "190")
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "193")
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "194") ))))
      ))))
    ))))
  )


(defnt "183"
  ;; public concrete syntax
  (var nil (must (once (seq
    "PUBLIC"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "74")
    (var nil (opt (once (seq
      (var nil (must (many (seq
        (nt "65") ))))
      "SWITCHES"
      (var nil (must (many (seq
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "64")
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "64") ))))
      ))))
    ))))
  )


(defnt "184"
  ;; shunned character number identification
  (var nil (must (once (seq
    "SHUNCHAR"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NONE"
      (var nil (must (once (seq
        (var nil (must (once (alt
          "CONTROLS" (nt "64") ))))
        (var nil (opt (many (seq
          (var nil (must (many (seq
            (nt "65") ))))
          (nt "64") ))))
        ))))
      ))))
    ))))
  )


(defnt "185"
  ;; syntax-reference character set
  (var nil (must (once (seq
    (nt "173+") ))))
  )


(defnt "186"
  ;; function character identification
  (var nil (must (once (seq
    "FUNCTION"
    (var nil (must (many (seq
      (nt "65") ))))
    "RE"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "64")
    (var nil (must (many (seq
      (nt "65") ))))
    "RS"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "64")
    (var nil (must (many (seq
      (nt "65") ))))
    "SPACE"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "64")
    (var nil (opt (many (seq
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "187")
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "188")
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "64") ))))
    ))))
  )


(defnt "187"
  ;; added function
  (var nil (must (once (seq
    (nt "55") ))))
  )


(defnt "188"
  ;; function class
  (var nil (must (once (alt
    "FUNCHAR" "MSICHAR" "MSOCHAR" "MSSCHAR" "SEPCHAR" ))))
  )


(defnt "189"
  ;; naming rules
  (var nil (must (once (seq
    "NAMING"
    (var nil (must (many (seq
      (nt "65") ))))
    "LCNMSTRT"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "66")
    (var nil (must (many (seq
      (nt "65") ))))
    "UCNMSTRT"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "66")
    (var nil (must (many (seq
      (nt "65") ))))
    "LCNMCHAR"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "66")
    (var nil (must (many (seq
      (nt "65") ))))
    "UCNMCHAR"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "66")
    (var nil (must (many (seq
      (nt "65") ))))
    "NAMECASE"
    (var nil (must (many (seq
      (nt "65") ))))
    "GENERAL"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NO" "YES" ))))
    (var nil (must (many (seq
      (nt "65") ))))
    "ENTITY"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NO" "YES" ))))
    ))))
  )


(defnt "190"
  ;; delimiter set
  (var nil (must (once (seq
    "DELIM"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "191")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "192") ))))
  )


(defnt "191"
  ;; general delimiters
  (var nil (must (once (seq
    "GENERAL"
    (var nil (must (many (seq
      (nt "65") ))))
    "SGMLREF"
    (var nil (opt (many (seq
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "55")
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "66") ))))
    ))))
  )


(defnt "192"
  ;; short reference delimiters
  (var nil (must (once (seq
    "SHORTREF"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "SGMLREF" "NONE" ))))
    (var nil (opt (many (seq
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "66") ))))
    ))))
  )


(defnt "193"
  ;; reserved name use
  (var nil (must (once (seq
    "NAMES"
    (var nil (must (many (seq
      (nt "65") ))))
    "SGMLREF"
    (var nil (opt (many (seq
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "55")
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "66") ))))
    ))))
  )


(defnt "194"
  ;; quantity set
  (var nil (must (once (seq
    "QUANTITY"
    (var nil (must (many (seq
      (nt "65") ))))
    "SGMLREF"
    (var nil (opt (many (seq
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "55")
      (var nil (must (many (seq
        (nt "65") ))))
      (nt "56") ))))
    ))))
  )


(defnt "195"
  ;; feature use
  (var nil (must (once (seq
    "FEATURES"
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "196")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "197+")
    (var nil (must (many (seq
      (nt "65") ))))
    (nt "198") ))))
  )


(defnt "196"
  ;; markup minimization features
  (var nil (must (once (seq
    "MINIMIZE"
    (var nil (must (many (seq
      (nt "65") ))))
    "DATATAG"
    (var nil (must (many (seq
      (nt "65") ))))
    (var datatag (must (once (alt
      "NO" "YES" ))))
    (var nil (must (many (seq
      (nt "65") ))))
    "OMITTAG"
    (var nil (must (many (seq
      (nt "65") ))))
    (var omittag (must (once (alt
      "NO" "YES" ))))
    (var nil (must (many (seq
      (nt "65") ))))
    "RANK"
    (var nil (must (many (seq
      (nt "65") ))))
    (var rank (must (once (alt
      "NO" "YES" ))))
    (var nil (must (many (seq
      (nt "65") ))))
    "SHORTTAG"
    (var nil (must (many (seq
      (nt "65") ))))
    (var shorttag (must (once (alt
      "NO" "YES" ))))
    ))))
  )


(defnt "197+"
  ;; link type features
  (var nil (must (once (seq
    "LINK"
    (var nil (must (many (seq
      (nt "65") ))))
    "SIMPLE"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NO"
      (var nil (must (once (seq
        "YES"
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "56") ))))
      ))))
    (var nil (must (many (seq
      (nt "65") ))))
    "IMPLICIT"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NO" "YES" ))))
    (var nil (must (many (seq
      (nt "65") ))))
    "EXPLICIT"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NO"
      (var nil (must (once (seq
        "YES"
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "56") ))))
      ))))
    ))))
  )


(defnt "198"
  ;; other features
  (var nil (must (once (seq
    "OTHER"
    (var nil (must (many (seq
      (nt "65") ))))
    "CONCUR"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NO"
      (var nil (must (once (seq
        "YES"
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "56") ))))
      ))))
    (var nil (must (many (seq
      (nt "65") ))))
    "SUBDOC"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NO"
      (var nil (must (once (seq
        "YES"
        (var nil (must (many (seq
          (nt "65") ))))
        (nt "56") ))))
      ))))
    (var nil (must (many (seq
      (nt "65") ))))
    "FORMAL"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NO" "YES" ))))
    ))))
  )


(defnt "199"
  ;; application-specific information
  (var nil (must (once (seq
    "APPINFO"
    (var nil (must (many (seq
      (nt "65") ))))
    (var nil (must (once (alt
      "NONE" (nt "76") ))))
    ))))
  )

;;; fum.el ends here
