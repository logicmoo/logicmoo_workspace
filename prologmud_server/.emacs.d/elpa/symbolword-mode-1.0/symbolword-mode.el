;; -*- lexical-binding: t -*-
;; Key     Word oriented command      Symbolword oriented command
;; ============================================================
;; M-f     `forward-word'             `forward-symbolword'
;; M-b     `backward-word'            `backward-symbolword'
;; M-@     `mark-word'                `mark-symbolword' ;no implementation
;; M-d     `kill-word'                `kill-symbolword'
;; M-DEL   `backward-kill-word'       `backward-kill-symbolword'
;; M-t     `transpose-words'          `transpose-symbolword' ;no implementation
;; M-c     `capitalize-word'          `capitalize-symbolword' ;no implementation
;; M-u     `upcase-word'              `upcase-symbolword' ;no implementation
;; M-l     `downcase-word'            `downcase-symbolword' ;no implementation

(require 'symbolword-ward)
(require 'symbolword-kill)

(defvar symbolword-mode-map (make-sparse-keymap))

(define-key symbolword-mode-map [remap forward-word]       'forward-symbolword)
(define-key symbolword-mode-map [remap backward-word]      'backward-symbolword)
(define-key symbolword-mode-map [remap kill-word]          'kill-symbolword)
(define-key symbolword-mode-map [remap backward-kill-word] 'backward-kill-symbolword)

(easy-mmode-define-minor-mode symbolword-mode "Grab keys" t " SW" symbolword-mode-map)

(provide 'symbolword-mode)
