(defun kmax-split-string (string separator)
 (split-string string separator nil nil))

(defun kmax-pp-string (string)
 ""
 (kmax-edit-temp-file)
 (insert string)
 (pp-buffer)
 (buffer-substring-no-properties)
 (kill-buffer))

(defun kmax-string-match-p (regexp string)
 (string-match-p regexp string))

(defun kmax-edit-temp-file ()
 ""
 (interactive)
 (ffap (make-temp-file "/tmp/kmax-tmp-file-")))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
   (buffer-string)))

(defun see (data &optional duration)
 ""
 (interactive)
 (message (prin1-to-string data))
 (sit-for (if duration duration 2.0))
 data)

(defun non-nil (arg)
 (if (symbolp arg)
  (and (boundp arg)
   (not (equal arg nil)))
  t))
