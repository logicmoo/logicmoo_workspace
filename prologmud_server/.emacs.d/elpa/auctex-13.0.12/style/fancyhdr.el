;;; fancyhdr.el --- AUCTeX style for `fancyhdr.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2013, 2018-2021 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `fancyhdr.sty', v4.0 from 2021/01/04.

;;; Code:

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

;; Because there can be many places, `TeX-completing-read-multiple' is
;; used instead of just `completing-read', and a `collection' argument
;; is provided as the list of places differs between the macros
(defun TeX-arg-fancyhdr-place (optional
                               &optional prompt collection full)
  "Prompt for fancyhdr places with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  If non-nil, PROMPT is
used as the prompt.  If non-nil, COLLECTION is used as the
completion list for the place.

If FULL is non-nil, a full list of places is offered for
completion, otherwise a reduced one omitting place combinations
for H(eader) or F(ooter)."
  (let* ((places (or collection
                     ;; Standard places with no restrictions.
                     ;; Lower-case versions, and reverse versions
                     ;; (e.g., OC) are left out for simplicity.
                     (if full
                         '("L" "LO" "LE" "LOH" "LOF" "LEH" "LEF"
                           "C" "CO" "CE" "COH" "COF" "CEH" "CEF"
                           "R" "RO" "RE" "ROH" "ROF" "REH" "REF")
                       '("L" "LO" "LE" "C" "CO" "CE" "R" "RE" "RO"))))
         (arguments (mapconcat #'identity
                               (TeX-completing-read-multiple
                                (TeX-argument-prompt optional
                                                     prompt
                                                     "Places")
                                places)
                               ",")))
    (TeX-argument-insert arguments optional)))

(defvar LaTeX-fancyhdr-fancypagestyle-regexp
  '("\\\\fancypagestyle{\\([^}]+\\)}"
    1 LaTeX-auto-pagestyle)
  "Regexp matching the first argument of \\fancypagestyle macro.")

(TeX-add-style-hook
 "fancyhdr"
 (lambda ()
   (TeX-add-symbols

    ;; 2 Using fancyhdr
    '("fancyhead" [ TeX-arg-fancyhdr-place ] t)
    '("fancyfoot" [ TeX-arg-fancyhdr-place ] t)
    '("fancyhf"   [ (TeX-arg-fancyhdr-place nil nil t) ] t)

    '("fancyheadoffset"
      [ (TeX-arg-fancyhdr-place nil ("L" "LO" "LE" "R" "RO" "RE")) ]
      TeX-arg-length)
    '("fancyfootoffset"
      [ (TeX-arg-fancyhdr-place nil ("LO" "LE" "L" "RO" "RE" "R")) ]
      TeX-arg-length)
    '("fancyhfoffset"
      [ (TeX-arg-fancyhdr-place nil ("L" "LO" "LE" "LOH" "LOF" "LEH" "LEF"
                                     "R" "RO" "RE" "ROH" "ROF" "REH" "REF")) ]
      TeX-arg-length)

    "headrulewidth" "footrulewidth"
    "headruleskip"  "footruleskip"
    "headrule"      "footrule"
    "headwidth"

    '("fancyheadinit" t)
    '("fancyfootinit" t)
    '("fancyhfinit"   t)

    '("fancycenter"
      [ TeX-arg-length "Distance" ] [ "Stretch" ] 3)

    '("iftopfloat"  2)
    '("ifbotfloat"  2)
    '("iffloatpage" 2)
    '("iffootnote"  2)

    '("fancypagestyle"
      ;; Always add the chosen pagestyle to list of known pagestyles,
      ;; dupes are removed when retrieving with the function
      ;; `LaTeX-pagestyle-list':
      (TeX-arg-pagestyle nil t)
      [ TeX-arg-pagestyle "Base pagestyle" ]
      t)

    ;; 15 The scoop on LATEX’s marks
    '("nouppercase" t))

   ;; 30 Deprecated commands
   ;; Don't offer deprecated commands in V4.0 for completion anymore.
   ;; '("lhead" t)
   ;; '("lfoot" t)
   ;; '("chead" t)
   ;; '("cfoot" t)
   ;; '("rhead" t)
   ;; '("rfoot" t)
   ;; "plainfootrulewidth"
   ;; "plainheadrulewidth"

   ;; `fancyhdr.sty' supplies these two pagestyles.  Pagestyle
   ;; `fancyplain' is now deprecated.
   (LaTeX-add-pagestyles "fancy" "fancydefault")

   ;; Add \fancypagestyle{pagestyle} to AUCTeX parser
   (TeX-auto-add-regexp LaTeX-fancyhdr-fancypagestyle-regexp)

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fancyhead" "[{")
                                ("fancyfoot" "[{")
                                ("fancyhf"   "[{")
                                ("fancyheadoffset" "[{")
                                ("fancyfootoffset" "[{")
                                ("fancyhfoffset"   "[{")
                                ("fancyheadinit"   "{")
                                ("fancyfootinit"   "{")
                                ("fancyhfinit"     "{")
                                ;; Fontify deprecated commands for
                                ;; older documents; to be removed
                                ;; sometimes ...
                                ("lhead" "[{")
                                ("lfoot" "[{")
                                ("chead" "[{")
                                ("cfoot" "[{")
                                ("rhead" "[{")
                                ("rfoot" "[{")
                                ;; Don't fontify the last argument;
                                ;; all macros used there should have
                                ;; their own fontification since they
                                ;; can also be used in a document
                                ;; top-level.
                                ("fancypagestyle"  "{["))
                              'function)))
 TeX-dialect)

(defvar LaTeX-fancyhdr-package-options
  '("nocheck" "compatV3" "headings" "myheadings")
  "Package options for fancyhdr package.")

;;; fancyhdr.el ends here
