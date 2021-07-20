;;; web-mode-edit-element-utils.el

;; Copyright 2016 Julian T. Knabenschuh

;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

(defun web-mode-edit-element-utils-x-position (fx)
  (save-excursion
    (funcall fx)
    (point)))

(defun web-mode-edit-element-utils-fnil (val f)
  (if val val
    (funcall f)))

(defun web-mode-edit-element-utils-kill-region (&optional begin end)
  (let ((begin (web-mode-edit-element-utils-fnil begin 'region-beginning))
        (end (web-mode-edit-element-utils-fnil end 'region-end)))
    (let ((content (buffer-substring begin end)))
      (delete-region begin end)
      content)))

(provide 'web-mode-edit-element-utils)
;;; web-mode-edit-element-utils.el ends here
