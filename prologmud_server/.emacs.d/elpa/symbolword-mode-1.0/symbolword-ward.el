;; -*- lexical-binding: t -*-
(require 'symbolword-get)

(defun forward-symbolword ()
  (interactive)
  (if (eobp)
      ()
    (forward-char (-
                   (symbolword/get-div-word-count-forward (1+ (point)))
                   (point)))))

(defun symbolword/get-div-word-count-forward (n)
  (if (symbolword/div-word-point? n)
      n
    (symbolword/get-div-word-count-forward (1+ n))))

(defun backward-symbolword ()
  (interactive)
  (if (bobp)
      ()
    (backward-char (-
                    (point)
                    (symbolword/get-div-word-count-backward (1- (point)))))))

(defun symbolword/get-div-word-count-backward (n)
  (if (symbolword/div-word-point? n)
      n
    (symbolword/get-div-word-count-backward (1- n))))

(provide 'symbolword-ward)
