;;; sisu-mode.el --- Major mode for SiSU markup text

;; Copyright (C) 2011, 2016  Free Software Foundation, Inc.

;; Author: Ralph Amissah & Ambrose Kofi Laing
;; Maintainer: Ralph Amissah <ralph.amissah@gmail.com>
;; Keywords: text, syntax, processes, tools
;; Version:   7.1.8
;; URL: http://www.sisudoc.org/
;; originally looked at (based on) doc-mode, with kind permission of the author
;;   Author: SUN, Tong <suntong001@users.sf.net>, (c)2001-6, all right reserved
;;   Version: $Date: 2006/01/19 03:13:41 $ $Revision: 1.14 $
;;   Home URL: http://xpt.sourceforge.net/
;; with contributions from Kevin Ryde and Stefan Monnier

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Viva Software Libre!
;; Support the free software movement!
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;; SiSU (http://www.sisudoc.org/) is a document structuring and
;; publishing framework.  This package provides an Emacs major mode
;; for SiSU markup.

;; When this package is installed, files ending in ".sisu" are
;; automatically associated with sisu-mode.  If a file doesn't have a
;; .sisu extension, add a first line:
;; # -*- Sisu -*-

;; The documentation for the "Structure Of The Hierarchy Text" can be
;; found in the sisustring for the sisu-mode function.

;;; Code:

;;{{{ Variables:

(defgroup sisu-faces nil
  "AsciiSisu highlighting"
  :group 'sisus)

;; == Colors
; color n is more prominent than color n+1

(defface sisu-title-1-face
  `((((class color)
      (background dark))
     (:foreground "brown3" :bold t :height 1.2 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "brown3" :bold t :height 1.2 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiSisu titles at level 1."
  :group 'sisu-faces)

(defface sisu-title-2-face
  `((((class color)
      (background dark))
     (:foreground "yellow4" :bold t :height 1.1 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "yellow4" :bold t :height 1.1 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiSisu titles at level 2."
  :group 'sisu-faces)

(defface sisu-title-3-face
  `((((class color)
      (background dark))
     (:foreground "sienna3" :bold t))
    (((class color)
      (background light))
     (:foreground "sienna3" :bold t))
    (t (:weight bold)))
  "Face for AsciiSisu titles at level 3."
  :group 'sisu-faces)

(defface sisu-title-4-face
  `((((class color)
      (background dark))
     (:foreground "burlywood3"))
    (((class color)
      (background light))
     (:foreground "burlywood3"))
    (t ()))
  "Face for AsciiSisu titles at level 4."
  :group 'sisu-faces)

(defface info-node
  '((((class color) (background light)) (:foreground "brown" :bold t :italic t))
    (((class color) (background dark)) (:foreground "white" :bold t :italic t))
    (t (:bold t :italic t)))
  "Face for Info node names."
  :group 'sisu-faces)

(defvar sisu-title-1 'sisu-title-1-face)
(defvar sisu-title-2 'sisu-title-2-face)
(defvar sisu-title-3 'sisu-title-3-face)
(defvar sisu-title-4 'sisu-title-4-face)

(defvar sisu-general-font-lock-red1 font-lock-warning-face)
(defvar sisu-general-font-lock-red2 font-lock-comment-face)
(defvar sisu-general-font-lock-red3 font-lock-string-face)

(defvar sisu-general-font-lock-green1 font-lock-type-face)
(defvar sisu-general-font-lock-green2 font-lock-constant-face)

(defvar sisu-general-font-lock-blue1 font-lock-keyword-face)
(defvar sisu-general-font-lock-blue2 font-lock-function-name-face)
(defvar sisu-general-font-lock-blue3 font-lock-builtin-face)

(defvar sisu-general-font-lock-yellow1 font-lock-variable-name-face)
(defvar sisu-general-font-lock-yellow2 font-lock-comment-face)

;; == sisu-mode settings

(defvar sisu-mode-hook nil
  "Normal hook run when entering Sisu Text mode.")

(defvar sisu-mode-abbrev-table nil
  "Abbrev table in use in Sisu-mode buffers.")
(define-abbrev-table 'sisu-mode-abbrev-table ())

(defconst sisu-font-lock-keywords
  (eval-when-compile
    (list
      ;;grouped text ---------
      ;(cons "^```[ ]code\\(.\\|\n\\)+?\n```\n"      'sisu-general-font-lock-red2)
      (cons "^```[ ]+code.*?$\\|^```$"  'sisu-general-font-lock-red2)
      (cons "^```[ ]+table.*?$\\|^```$" 'sisu-general-font-lock-red2)
      (cons "^```[ ]+group$\\|^```$"    'sisu-general-font-lock-red2)
      (cons "^```[ ]+block$\\|^```$"    'sisu-general-font-lock-red2)
      (cons "^```[ ]+poem$\\|^```$"     'sisu-general-font-lock-red2)
      (cons "^```[ ]+alt$\\|^```$"      'sisu-general-font-lock-red2)
      ;;grouped text ---------
      (cons "^group{\\|^}group"       'sisu-general-font-lock-red2)
      (cons "^block{\\|^}block"       'sisu-general-font-lock-red2)
      (cons "^code{\\|^}code"         'sisu-general-font-lock-red2)
      (cons "^poem{\\|^}poem"         'sisu-general-font-lock-red2)
      (cons "^alt{\\|^}alt"           'sisu-general-font-lock-red2)
      (cons "^table{.+\\|^}table"     'sisu-general-font-lock-red2)
      (cons "^{table[^}]+}"           'sisu-general-font-lock-red2)

      (list
        (concat
          "^\`\\{3\\}[ ]+code.*?$"
          "\\(.\\|\n\\)+?"
          "\`\\{3\\}$"
        )
        '(1 sisu-general-font-lock-red2 t)
        '(2 nil t)
        '(3 sisu-general-font-lock-red2 t)
      )
      (list
        (concat
          "^\`\\{3\\}[ ]+table.*?$"
          "\\(.\\|\n\\)+?"
          "\`\\{3\\}$"
        )
        '(1 sisu-general-font-lock-red2 t)
        '(2 nil t)
        '(3 sisu-general-font-lock-red2 t)
      )
      (list
        (concat
          "^\`\\{3\\}[ ]+\\(group\\|block\\|alt\\|poem\\)$"
          "\\(.\\|\n\\)+?"
          "^\`\\{3\\}$"
        )
        '(1 sisu-general-font-lock-red2 t)
        '(2 nil t)
        '(3 sisu-general-font-lock-red2 t)
      )

      ;; footnote/endnote ----
      ;(cons "\~{.+?}\~"  'sisu-general-font-lock-green1)
      (cons "\~{\\*\\*\\|\~{\\*\\|\~{\\|}\~"   'sisu-general-font-lock-red2)
      (cons "\~\\[\\+\\|\~\\[\\*\\|\~\\[\\|\\]\~"  'sisu-general-font-lock-red2)
      (cons "\~\\^ \\|^\\^\~ " 'sisu-general-font-lock-red2)
      (list
        (concat
          "\\(\*\~\\)"
          "\\([^ \r\t\n]+\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-blue2 t)
      )

      ;; emphasis (can be program configured to be bold italics or underscore)
      (list
        (concat
          "\\([*]{\\)"
          "\\([^}]+\\)"
          "\\(}[*]\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; bold ----------------
      (list
        (concat
          "\\([!]{\\)"
          "\\([^}]+\\)"
          "\\(}[!]\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )
      (cons "\\*[^ ]+\\*"               'sisu-general-font-lock-red1)
      (cons "^!_ .+"                    'sisu-general-font-lock-red1)

      ;; italics -------------
      (list
        (concat
          "\\([/]{\\)"
          "\\([^}]+\\)"
          "\\(}[/]\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-blue1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; underscore ----------
      (list
        (concat
          "\\([_]{\\)"
          "\\([^}]+\\)"
          "\\(\}[_]\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; monospace -----------
      (list
        (concat
          "\\([#]{\\)"
          "\\([^}]+\\)"
          "\\(}[#]\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; citation ------------
      (list
        (concat
          "\\([\"]{\\)"
          "\\([^}]+\\)"
          "\\(}[\"]\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; inserted text -------
      (list
        (concat
          "\\([\+]{\\)"
          "\\([^}]+\\)"
          "\\(}[\+]\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; strike through ------
      (list
        (concat
          "\\(\\-{\\)"
          "\\([^}]+\\)"
          "\\(}\\-\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; superscript ---------
      (list
        (concat
          "\\(\\^{\\)"
          "\\([^}]+\\)"
          "\\(}\\^\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; subscript -----------
      (list
        (concat
          "\\([,]{\\)"
          "\\([^}]+\\)"
          "\\(}[,]\\)"
        )
        '(1 sisu-general-font-lock-red1 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-red1 t)
      )

      ;; numbered list
      (cons "^# \\|^_# "                'sisu-general-font-lock-red1)

      ;; bullet text
      (cons "^_\\*[1-9] \\|^_\\* "      'sisu-general-font-lock-red1)

      ;; indented text
      (cons "^_[1-9] "                  'sisu-general-font-lock-red1)
      (cons "^_[1-9]! "                 'sisu-general-font-lock-red1)

      ;; hanging indented text [proposed enable when implemented]
      (cons "^__[1-9] "                'sisu-general-font-lock-red1)
      (cons "^_[0-9]_[0-9] "           'sisu-general-font-lock-red1)
      (cons "^__[1-9]! "               'sisu-general-font-lock-red1)
      (cons "^_[0-9]_[0-9]! "          'sisu-general-font-lock-red1)

      ;; url
      (cons "\\(^\\|[ ]\\)http:[/][/][^ \t\n\r<]+" 'sisu-general-font-lock-blue2)

      ;; Comment Lines
      (cons "^% .*"                     'sisu-general-font-lock-blue1)

      ;; page break
      (cons "^\\(-\\\\\\\\-\\|=\\\\\\\\=\\|-\\.\\.-\\)" 'sisu-general-font-lock-red2)

      ;; line break
      (cons " \\\\\\\\ "                'sisu-general-font-lock-red1)

      ;; line break (depreciated)
      (cons "<br>"                      'sisu-general-font-lock-red1)

      ;; Section titles
      (list "^\\(\\([1-4]\\|:?[A-D]\\)\\~\\)\\(.*\\)"
        '(1 sisu-title-1 t)
        '(3 sisu-title-2 t)
      )

      ;; hyper-links
      (list
        (concat
          "\\({~^\\|{\\)"
          "\\([^}{]+\\)"
          "\\(}http:[/][/][^ \r\n\t<]+\\)"
        )
        '(1 sisu-general-font-lock-blue2 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-blue2 t)
      )

      ;; book index
      (list
        (concat
          "^\\(\={\\)"
          "\\([^}{]+\\)"
          "\\(}\\)$"
        )
        '(1 sisu-general-font-lock-green1 t)
        '(2 nil t)
        '(3 sisu-general-font-lock-green1 t)
      )

      ;(cons "^\={.+}"                 'sisu-general-font-lock-green1)

      ;; numbers
      (cons "\\<[.0-9]+\\>"             'sisu-general-font-lock-green2)

      ;; bullets sisu_normal (nearly copied regexp)
      (cons "^_\\([1-9*]\\|[1-9]\\*\\) " 'sisu-general-font-lock-blue2)

      ;; image links
      (list
        (concat
          "\\({\\)"
          "\\([^}{]+\\)"
          "\\(}image\\)"
        )
        '(1 sisu-general-font-lock-blue2 t)
        '(2 sisu-general-font-lock-red1 t)
        '(3 sisu-general-font-lock-blue2 t)
      )

      ;; insert file links
      (list
        (concat
          "\\(<< \\)"
          "\\([^ \r\t\n]+\\.ss\\)"
          "\\(i\\|t\\)"
        )
        '(1 sisu-general-font-lock-blue2 t)
        '(2 sisu-general-font-lock-blue2 t)
        '(3 sisu-general-font-lock-blue2 t)
      )

      ;; raw keywords
      (list
        (concat
          "^\\(\\@\\("
          "creator\\|"
          "title\\|"
          "date\\|"
          "rights\\|"
          "publisher\\|"
          "classify\\|"
          "identifier\\|"
          "original\\|"
          "notes\\|"
          "links\\|"
          "make\\|"
          "\\):\\)\\(.*\\)"
        )
        '(1 sisu-title-2 keep)
        '(3 sisu-title-3 keep)
      )
    )
  )
  "Default expressions to highlight in AsciiSisu mode."
)

;; outline mode evil "folding" if available
;; (define-key evil-normal-state-map ",0"   'show-all)
;; (define-key evil-normal-state-map ",-"   'hide-body)
;; (define-key evil-normal-state-map ",+"   'show-subtree)
;; (define-key evil-normal-state-map ",="   'show-subtree)

;;}}}

;;{{{ Sisu & Autoload:

;;;###autoload
(define-derived-mode sisu-mode text-mode "SiSU"
  "Major mode for editing SiSU files.
SiSU document structuring, publishing in multiple formats and search.
URL `http://www.sisudoc.org/'"
  (modify-syntax-entry ?\'  ".")
  ;;(flyspell-mode nil)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|>" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)

  (set (make-local-variable 'outline-regexp)
       "^\\(\\([1-4]\\|:?[A-D]\\)\\~\\|\\@[a-z]+:\\( \\|$\\)\\)")

  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(sisu-font-lock-keywords
          nil                           ; KEYWORDS-ONLY: no
          nil                           ; CASE-FOLD: no
          ((?_ . "w"))                  ; SYNTAX-ALIST
          ))
  ;; Enable outlining.
  ;; TODO with outlining make sure linum (line numbering) is off,
  ;; else performance penalty, sucks bigtime
  (outline-minor-mode 1))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.ss[imt]\\'" . sisu-mode))

;;;; ChangeLog:

;; 2016-02-22  Ralph Amissah  <ralph.amissah@gmail.com>
;; 
;; 	* sisu-mode/sisu-mode.el: Clean up name space, clear sisu-mode-hook
;; 
;; 	* sisu-mode/sisu-mode.el (sisu-general-font-lock-*): Rename from 
;; 	general-font-lock-* to obey the package-prefix convention.
;; 	(sisu-font-lock-keywords): Adjust accordingly.
;; 	(sisu-mode-hook): Don't add outline-minor-mode.
;; 	(sisu-mode): Call it here instead.
;; 
;; 2016-02-21  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sisu-mode/sisu-mode.el: Fix Maintainer: address.
;; 
;; 2016-02-21  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/sisu-mode/sisu-mode.el: Update to 7.1.8
;; 
;; 	(sisu-font-lock-keywords): Reindent and improve.
;; 	(sisu-mode): Add outline-regexp.  Don't run sisu-mode-hook since 
;; 	define-derived-mode already does it for us.
;; 	(sisu-mode-hook): Add outline-minor-mode to its default.
;; 	(auto-mode-alist): Use .ss[imt] extensions rather than .sisu.
;; 
;; 2011-07-12  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Fix version numbers of sisu-mode, register-list, and windresize.
;; 
;; 2011-07-08  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	sisu-mode.el: Add .sisu to auto-mode-alist using autoload cookie. Minor
;; 	doc fixes.
;; 
;; 2011-07-06  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sisu-mode.el (sisu-mode): Autoload.
;; 
;; 2011-07-04  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add sisu-mode.el.  Update all.el licence.
;; 


(provide 'sisu-mode)

;;}}}

;;; sisu-mode.el ends here
