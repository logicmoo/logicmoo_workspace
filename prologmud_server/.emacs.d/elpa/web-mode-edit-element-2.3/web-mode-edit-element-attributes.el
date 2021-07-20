;;; web-mode-edit-element-attributes.el

;; Copyright 2016 Julian T. Knabenschuh

;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

(require 'web-mode)

(defun web-mode-edit-element-attributes-end-inside ()
  (interactive)
  (web-mode-attribute-end)
  (backward-char))

(defun web-mode-edit-element-attributes-transpose-backward ()
  (interactive)
  (save-excursion
    (web-mode-attribute-beginning)
    (web-mode-attribute-previous)
    (web-mode-attribute-transpose)))

(provide 'web-mode-edit-element-attributes)
;;; web-mode-edit-element-attributes.el ends here
