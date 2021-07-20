;;; orgnav-log.el --- Functions to allog orgnave to log


;;; Commentary:
;;

;;; Code:

(defvar orgnav-log nil "Whether orgnav should log.")

(defun orgnav-log-enable ()
  "Toggle orgnav logging."
  (interactive)
  (setq orgnav-log (not orgnav-log)))

(defun orgnav-log (format-string &rest args)
  "Print logging depending of ORGNAV-LOG variable.  FORMAT-STRING  and ARGS have the same meanings as message."
  (when orgnav-log
    (apply 'message format-string args)))

(provide 'orgnav-log)
;;; orgnav-log.el ends here
