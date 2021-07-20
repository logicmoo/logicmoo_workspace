;;; web-mode-edit-element.el --- Helper-functions for attribute- and element-handling

;; Copyright 2016 Julian T. Knabenschuh

;; Version: 2.3
;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; Homepage: https://github.com/jtkDvlp/web-mode-edit-element
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs
;; Keywords: languages convenience
;; Package-Requires: ((emacs "24.4") (web-mode "14"))

;;; Commentary:

;; "web-mode-edit-element" is a smart enhancement for the package web-mode inspired by the packages ParEdit and Paxedit.

;; It provides a few helper-functions for attribute- and element-handling based on the functions given by web-mode. Further more it provides functions for slurping, barfing, dissolving, raising ... elements inspired by ParEdit and Paxedit. Last but not least this package includes a minor mode to provide a keymap with default bindings using commands of web-mode and this package.

;; To use this package, add the following lines somewhere in you init file:
;; (require 'web-mode-edit-element)
;; (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode)

;; See keymap in the main file or online https://github.com/jtkDvlp/web-mode-edit-element

;;; Code:

(require 'web-mode)
(require 'web-mode-edit-element-attributes)
(require 'web-mode-edit-element-elements)

(defvar web-mode-edit-element-minor-mode-map
  (make-keymap)
  "web-mode-edit-element-minor-mode keymap")

;;;###autoload
(define-minor-mode web-mode-edit-element-minor-mode
  "Minor mode to provide key-bindings for web-mode-edit-element functions"
  nil " web-mode-edit-element" 'web-mode-edit-element-minor-mode-map)

(let ((bindings
       '(;; General
         ("C-(" web-mode-element-wrap)
         ("M-(" web-mode-element-rename)

         ("C-M-SPC" web-mode-element-content-select)

         ;; Elements
         ("C-<left>" web-mode-element-previous)
         ("C-<right>" web-mode-element-next)

         ("M-<left>" web-mode-edit-element-elements-contract-over-border)
         ("M-<right>" web-mode-edit-element-elements-expand-over-border)

         ("C-M-<left>" web-mode-edit-element-elements-transpose-backward)
         ("C-M-<right>" web-mode-element-transpose)

         ("C-<up>" web-mode-element-beginning)
         ("C-<down>" web-mode-tag-match)

         ("C-S-<up>" web-mode-element-parent)
         ("C-S-<down>" web-mode-element-next)

         ("M-<up>" web-mode-edit-element-elements-dissolve)
         ("M-<down>" web-mode-edit-element-elements-raise)

         ("C-M-<up>" web-mode-element-vanish)
         ("C-M-<down>" web-mode-edit-element-elements-sibling-next-or-parent)

         ("C-k" web-mode-element-kill)
         ("C-S-k" web-mode-edit-element-elements-kill-siblings)
         ("M-k" web-mode-edit-element-elements-kill-siblings-previous)
         ("M-K" web-mode-edit-element-elements-kill-siblings-next)

         ;; Attributes
         ("C-S-<left>" web-mode-attribute-previous)
         ("C-S-<right>" web-mode-attribute-next)

         ("C-M-S-<left>" web-mode-edit-element-attributes-transpose-backward)
         ("C-M-S-<right>" web-mode-attribute-transpose)

         ("C-M-S-<up>" web-mode-attribute-beginning)
         ("C-M-S-<down>" web-mode-edit-element-attributes-end-inside)

         ("C-M-K" web-mode-attribute-kill))))
  (dolist (binding bindings)
    (define-key web-mode-edit-element-minor-mode-map
      (kbd (car binding))
      (car (cdr binding))))

  (provide 'web-mode-edit-element))
;;; web-mode-edit-element.el ends here
