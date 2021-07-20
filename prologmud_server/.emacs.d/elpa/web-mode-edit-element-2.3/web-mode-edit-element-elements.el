;;; web-mode-edit-element-elements.el

;; Copyright 2016 Julian T. Knabenschuh

;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Code:

(require 'web-mode)
(require 'web-mode-edit-element-utils)
(require 'subr-x)

;; General
(defun web-mode-edit-element-elements-end-inside ()
  (interactive)
  (web-mode-element-end)
  (backward-char))

;; Insert
(defun web-mode-edit-element-elements-insert-direct-before (content)
  (interactive "sContent: ")
  (save-excursion
    (web-mode-tag-beginning)
    (insert content)))

(defun web-mode-edit-element-elements-insert-before (content)
  (interactive "sContent: ")
  (save-excursion
    (web-mode-element-beginning)
    (web-mode-tag-previous)
    (web-mode-edit-element-elements-insert-direct-after content)))

(defun web-mode-edit-element-elements-insert-direct-after (content)
  (interactive "sContent: ")
  (save-excursion
    (web-mode-tag-end)
    (insert content)))

(defun web-mode-edit-element-elements-insert-after (content)
  (interactive "sContent: ")
  (save-excursion
    (web-mode-edit-element-elements-end-inside)
    (web-mode-tag-next)
    (web-mode-edit-element-elements-insert-direct-before content)))

;; Sibling
(defun web-mode-edit-element-elements-sibling-previous-position ()
  (web-mode-edit-element-utils-x-position
   'web-mode-element-sibling-previous))

(defun web-mode-edit-element-elements-sibling-previous-p ()
  (let ((parent-position
         (web-mode-edit-element-utils-fnil
          (save-excursion
            (web-mode-element-beginning)
            (web-mode-element-parent-position))
          'point))

        (tag-prev-position
         (web-mode-edit-element-utils-x-position
          (lambda ()
            (web-mode-element-beginning)
            (web-mode-tag-previous)
            (web-mode-element-beginning)))))

    (not (= parent-position tag-prev-position))))

(defun web-mode-edit-element-elements-sibling-next-position ()
  (web-mode-edit-element-utils-x-position
   'web-mode-element-sibling-next))

(defun web-mode-edit-element-elements-sibling-next-p ()
  (let ((parent-position
         (web-mode-edit-element-utils-fnil
          (save-excursion
            (web-mode-element-beginning)
            (web-mode-element-parent-position))
          'point))

        (tag-next-position
         (web-mode-edit-element-utils-x-position
          (lambda ()
            (web-mode-edit-element-elements-end-inside)
            (web-mode-tag-next)
            (web-mode-element-beginning)))))

    (not (= parent-position tag-next-position))))

(defun web-mode-edit-element-elements-sibling-next-or-parent ()
  (interactive)
  (if (web-mode-edit-element-elements-sibling-next-p)
      (web-mode-element-sibling-next)
    (web-mode-element-parent)))

;; Parent
(defun web-mode-edit-element-elements-parent-p ()
  (save-excursion
    (web-mode-element-beginning)
    (and (web-mode-element-parent-position)
         (not (= (web-mode-element-parent-position)
                 (web-mode-element-beginning-position))))))

(defun web-mode-edit-element-elements-root-p ()
  (not (web-mode-edit-element-elements-parent-p)))

;; Child
(defun web-mode-edit-element-elements-child-p ()
  (let ((end-tag-position
         (web-mode-edit-element-utils-x-position
          (lambda ()
            (web-mode-edit-element-elements-end-inside)
            (web-mode-tag-beginning))))

        (child-position
         (save-excursion
           (web-mode-element-beginning)
           (web-mode-tag-next-position))))

    (not (= end-tag-position child-position))))

(defun web-mode-edit-element-elements-child-last ()
  (interactive)
  (if (web-mode-edit-element-elements-child-p)
      (progn
        (web-mode-edit-element-elements-end-inside)
        (web-mode-tag-beginning)
        (web-mode-tag-previous))
    (progn
      (web-mode-element-beginning)
      (web-mode-tag-end))))

(defun web-mode-edit-element-elements-child-first ()
  (interactive)
  (if (web-mode-edit-element-elements-child-p)
      (progn
        (web-mode-element-beginning)
        (web-mode-tag-next))
    (progn
      (web-mode-element-beginning)
      (web-mode-tag-end))))

;; Kill
(defun web-mode-edit-element-elements-kill-siblings-previous ()
  (interactive)
  (save-excursion
    (web-mode-element-beginning)
    (set-mark (point))
    (if (web-mode-edit-element-elements-parent-p)
        (progn
          (web-mode-element-parent)
          (web-mode-tag-end))
      (beginning-of-buffer))
    (kill-region
     (region-beginning)
     (region-end))
    (insert "\n")))

(defun web-mode-edit-element-elements-kill-siblings-next ()
  (interactive)
  (save-excursion
    (set-mark (+ 1 (web-mode-element-end-position)))
    (if (web-mode-edit-element-elements-parent-p)
        (progn
          (web-mode-element-beginning)
          (web-mode-element-parent)
          (web-mode-edit-element-elements-end-inside)
          (web-mode-tag-beginning))
      (end-of-buffer))
    (kill-region
     (region-beginning)
     (region-end))
    (insert "\n")))

(defun web-mode-edit-element-elements-kill-siblings ()
  (interactive)
  (web-mode-edit-element-elements-kill-siblings-previous)
  (web-mode-edit-element-elements-kill-siblings-next))

;; Edit
(defun web-mode-edit-element-elements-transpose-backward ()
  (interactive)
  (when (web-mode-edit-element-elements-sibling-previous-p)
    (save-excursion
      (web-mode-element-sibling-previous)
      (web-mode-element-transpose))))

(defun web-mode-edit-element-elements-expand-p ()
  (web-mode-edit-element-elements-sibling-next-p))

(defun web-mode-edit-element-elements-expand ()
  (interactive)
  (when (web-mode-edit-element-elements-expand-p)
    (let ((content
           (concat
            (string-trim-left
             (save-excursion
               (web-mode-element-end)
               (set-mark (point))
               (web-mode-tag-next)
               (web-mode-element-end)
               (web-mode-edit-element-utils-kill-region)))
            "\n")))
      (save-excursion
        (web-mode-edit-element-elements-end-inside)
        (web-mode-tag-beginning)
        (insert content)))))

(defun web-mode-edit-element-elements-expand-over-border ()
  (interactive)
  (save-excursion
    (while (and (web-mode-edit-element-elements-parent-p)
                (not (web-mode-edit-element-elements-expand-p)))
      (web-mode-element-parent))
    (web-mode-edit-element-elements-expand)))

(defun web-mode-edit-element-elements-contract-p ()
  (web-mode-edit-element-elements-child-p))

(defun web-mode-edit-element-elements-contract ()
  (interactive)
  (when (web-mode-edit-element-elements-contract-p)
    (let ((content
           (save-excursion
             (web-mode-edit-element-elements-child-last)
             (web-mode-element-beginning)
             (web-mode-tag-previous)
             (web-mode-tag-end)
             (set-mark (point))
             (web-mode-tag-next)
             (web-mode-element-end)
             (web-mode-edit-element-utils-kill-region))))

      (save-excursion
        (web-mode-element-end)
        (web-mode-edit-element-elements-insert-direct-after content)))))

(defun web-mode-edit-element-elements-contract-over-border ()
  (interactive)
  (save-excursion
    (while (and (web-mode-edit-element-elements-parent-p)
                (not (web-mode-edit-element-elements-contract-p)))
      (web-mode-element-parent))
    (web-mode-edit-element-elements-contract)))

(defun web-mode-edit-element-elements-dissolve (&optional ARGS)
  (interactive "p")
  (when (web-mode-edit-element-elements-parent-p)
    (save-excursion
      (web-mode-element-beginning)
      (web-mode-element-parent)
      (web-mode-element-vanish ARGS))))

(defun web-mode-edit-element-elements-raise (&optional ARGS)
  (interactive "p")
  (web-mode-edit-element-elements-kill-siblings)
  (web-mode-edit-element-elements-dissolve ARGS))

(provide 'web-mode-edit-element-elements)
;;; web-mode-edit-element-elements.el ends here
