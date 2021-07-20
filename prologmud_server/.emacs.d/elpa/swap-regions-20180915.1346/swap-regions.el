;;; swap-regions.el --- Swap text in two regions  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: convenience
;; Package-Version: 20180915.1346
;; Package-Commit: f4fd9880cf690e003fcde88dcf2b46adbbbb03cd
;; Homepage: https://github.com/xuchunyang/swap-regions.el
;; Package-Requires: ((emacs "24.3"))
;; Version: 0

;; This file is not part of GNU Emacs.  However, it is distributed under the
;; same license.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; You can swap text in two regions using
;;
;;   M-x swap-regions [select the first region] C-M-c [select the second region] C-M-c
;;
;; Note that C-M-c runs `exit-recursive-edit' which is bound
;; by default in vanilla Emacs.  And while you are selecting regions, you
;; can run any Emacs command thanks to (info "(elisp) Recursive Editing")

;;; Code:

(require 'cl-lib)

(defgroup swap-regions nil
  "Swap text in regions."
  :group 'convenience)

(defface swap-regions-selection '((t (:inherit secondary-selection)))
  "Face marking the previous selected region."
  :group 'swap-regions)

;;;###autoload
(defun swap-regions (buf-A reg-A-beg reg-A-end buf-B reg-B-beg reg-B-end)
  "Swap text in two regions."
  (interactive
   (let ((hint
          (substitute-command-keys
           "Finish `\\[exit-recursive-edit]', abort \
`\\[abort-recursive-edit]'"))
         buf-A reg-A-beg reg-A-end
         buf-B reg-B-beg reg-B-end
         buf-A-overlay)
     ;; Select the first region
     (unless (use-region-p)
       (let (message-log-max)
         (message "Select the first region (%s)" hint))
       (recursive-edit))
     (setq buf-A (current-buffer)
           reg-A-beg (region-beginning)
           reg-A-end (region-end))
     (deactivate-mark)
     (setq buf-A-overlay (make-overlay reg-A-beg reg-A-end))
     (overlay-put buf-A-overlay 'face 'swap-regions-selection)
     ;; Select the second region
     (let (message-log-max)
       (message "Select the second region (%s)" hint))
     (unwind-protect (recursive-edit)
       (delete-overlay buf-A-overlay))
     (setq buf-B (current-buffer)
           reg-B-beg (region-beginning)
           reg-B-end (region-end))
     (deactivate-mark)
     (list buf-A reg-A-beg reg-A-end buf-B reg-B-beg reg-B-end)))
  ;; Swap these two regions
  (let ((reg-A-str (with-current-buffer buf-A
                     (buffer-substring reg-A-beg reg-A-end)))
        (reg-B-str (with-current-buffer buf-B
                     (buffer-substring reg-B-beg reg-B-end))))
    (when (< reg-B-beg reg-A-beg)
      (cl-psetq buf-A buf-B
                reg-A-beg reg-B-beg
                reg-A-end reg-B-end
                reg-A-str reg-B-str
                buf-B buf-A
                reg-B-beg reg-A-beg
                reg-B-end reg-A-end
                reg-B-str reg-A-str))
    (with-current-buffer buf-B
      (delete-region reg-B-beg reg-B-end)
      (goto-char reg-B-beg)
      (insert reg-A-str))
    (with-current-buffer buf-A
      (delete-region reg-A-beg reg-A-end)
      (goto-char reg-A-beg)
      (insert reg-B-str))))

(provide 'swap-regions)

;; Local Variables:
;; fill-column: #x50
;; indent-tabs-mode: nil
;; End:

;;; swap-regions.el ends here
