;;; footmisc.el --- AUCTeX style for `footmisc.sty'  -*- lexical-binding: t; -*-

;; Copyright (C) 2011, 2018--2021 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Created: 2011-04-08
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

;; This file adds support for `footmisc.sty' (v5.5b) from 2011/06/06.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "footmisc"
 (lambda ()
   (TeX-add-symbols

    ;; 1.4 Option ragged and \footnotelayout
    "footnotelayout"

    ;; 1.7 The \setfnsymbol and \DefineFNsymbols commands
    '("DefineFNsymbols"
      (TeX-arg-eval completing-read
                    (TeX-argument-prompt nil nil "Name")
                    '("bringhurst" "chicago" "wiley"
                      "lamport" "lamport*"))
      [TeX-arg-eval completing-read
                    (TeX-argument-prompt t nil "Style (text or math)")
                    '("text" "math")]
      1)
    '("DefineFNsymbols*"
      (TeX-arg-eval completing-read
                    (TeX-argument-prompt nil nil "Name")
                    '("bringhurst" "chicago" "wiley"
                      "lamport" "lamport*"))
      [TeX-arg-eval completing-read
                    (TeX-argument-prompt t nil "Style (text or math)")
                    '("text" "math")]
      1)

    ;; These two commands define both text and math variants of the
    ;; footnote symbols
    '("DefineFNsymbolsTM"
      (TeX-arg-eval completing-read
                    (TeX-argument-prompt nil nil "Name")
                    '("bringhurst" "chicago" "wiley"
                      "lamport" "lamport*"))
      1)
    '("DefineFNsymbolsTM*"
      (TeX-arg-eval completing-read
                    (TeX-argument-prompt nil nil "Name")
                    '("bringhurst" "chicago" "wiley"
                      "lamport" "lamport*"))
      1)
    '("setfnsymbol"
      (TeX-arg-eval completing-read
                    (TeX-argument-prompt nil nil "Name")
                    '("bringhurst" "chicago" "wiley"
                      "lamport" "lamport*")))

    ;; 1.11 Option hang
    "hangfootparskip"
    "hangfootparindent"

    ;; 1.15 The multiple option
    "multiplefootnotemarker"
    "multfootsep"

    ;; 1.16 User interface
    ;; The following command references a label inside in a footnote
    '("footref" TeX-arg-ref)
    "mpfootnotemark")

   ;; 1.9 Option marginal
   (LaTeX-add-lengths "footnotemargin")

   ;; 1.13 Option splitrule
   (when (LaTeX-provided-package-options-member "footmisc" "splitrule")
     (TeX-add-symbols "mpfootnoterule"
                      "pagefootnoterule"
                      "splitfootnoterule"))

   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("DefineFNsymbols"   "*{[{")
                                ("DefineFNsymbolsTM" "*{{")
                                ("setfnsymbol"       "{"))
                              'function)
     (font-latex-add-keywords '(("footref"))
                              'reference)))
 TeX-dialect)

(defvar LaTeX-footmisc-package-options '("perpage" "side" "ragged"
                                         "para" "symbol" "symbol*"
                                         "marginal" "flushmargin" "hang"
                                         "norule" "splitrule" "stable"
                                         "multiple")
  "Package options for the footmisc package.")

;;; footmisc.el ends here
