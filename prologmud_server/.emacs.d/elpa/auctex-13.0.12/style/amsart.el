;;; amsart.el --- Style hook for the AMS-LaTeX article document class.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "amsart"
 (function
  (lambda ()
    (TeX-run-style-hooks "amsmath" "amsthm")
    (LaTeX-add-environments "abstract")))
 TeX-dialect)

;;; amsart.el ends here.
