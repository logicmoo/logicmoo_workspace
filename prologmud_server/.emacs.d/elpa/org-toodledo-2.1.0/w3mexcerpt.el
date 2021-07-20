;;; w3mexcerpt.el --- an EXCERPT from w3m.el Emacs interface to w3m
;; w3m requires many parts.  This excerpt only contains material needed for w3m-url-encode-string.
;; This is sufficient for org-toodledo
;; Excerpt made by Stoph Long 2010.

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>,
;;          Tsuyoshi CHO       <mfalcon_sky@emailuser.net>
;; Keywords: w3m, WWW, hypermedia

;; This file is the main part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Emacs-w3m is an Emacs interface to the w3m program.  For more
;; detail about w3m, see:
;;
;;    http://w3m.sourceforge.net/


(defvar w3m-type nil
  "Type of the w3m command.
The valid values include `w3m', `w3mmee', and `w3m-m17n'.")

(defcustom w3m-language
  (if (or (and (boundp 'current-language-environment)
	       (string= "Japanese"
			(symbol-value 'current-language-environment)))
	  (boundp 'MULE))
      "Japanese")
  "*Your preferred language used in emacs-w3m sessions."
  :group 'w3m
  :type '(radio (const :format "%v " "Japanese")
		(const :tag "Other" nil))
  :get (lambda (symbol)
	 (let ((value (format "%s" (default-value symbol)))
	       (case-fold-search t))
	   (prog1
	       (setq value (if (string-match "\\`japan" value) "Japanese"))
	     (set-default symbol value))))
  :set (lambda (symbol value)
	 (set-default symbol (if (equal value "Japanese") "Japanese"))))

(defcustom w3m-coding-system (if (featurep 'mule)
				 (if (eq w3m-type 'w3mmee)
				     'iso-2022-7bit-ss2
				   'iso-2022-7bit)
			       'iso-8859-1)
  "*Default coding system used to communicate with the w3m command."
  :group 'w3m
  :type '(coding-system :size 0))

(defcustom w3m-default-coding-system
  (if (equal "Japanese" w3m-language) 'shift_jis 'iso-8859-1)
  "*Default coding system used to encode url strings and post-data."
  :group 'w3m
  :type '(coding-system :size 0))

(defun w3m-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((char-equal ch ?\x20)	; space
	      "+")
	     (t
	      (format "%%%02x" ch))))	; escape
	  ;; Coerce a string into a list of chars.
	  (append (encode-coding-string (or str "")
					(or coding
					    w3m-default-coding-system
					    w3m-coding-system
					    'iso-2022-7bit))
		  nil))))

(provide 'w3mexcerpt)
