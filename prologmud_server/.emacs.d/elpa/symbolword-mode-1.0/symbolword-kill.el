;; -*- lexical-binding: t -*-
(require 'symbolword-get)
(require 'symbolword-ward)

(defun kill-symbolword ()
  (interactive)
  (if (or (eobp) (get-text-property (1+ (point)) 'read-only))
      ()
    (kill-forward-chars (-
                        (symbolword/get-div-word-count-forward-for-kill (1+ (point)))
                        (point)))))

(defun symbolword/get-div-word-count-forward-for-kill (n)
  (if (or (symbolword/div-word-point? n) (get-text-property n 'read-only))
      n
    (symbolword/get-div-word-count-forward (1+ n))))

(defun backward-kill-symbolword ()
  (interactive)
  (if (or (bobp) (get-text-property (1- (point)) 'read-only))
      ()
    (kill-backward-chars (-
                    (point)
                    (symbolword/get-div-word-count-backward (1- (point)))))))

(defun symbolword/get-div-word-count-backward (n)
  (if (or (symbolword/div-word-point? n) (get-text-property n 'read-only))
      n
    (symbolword/get-div-word-count-backward (1- n))))

(provide 'symbolword-kill)
