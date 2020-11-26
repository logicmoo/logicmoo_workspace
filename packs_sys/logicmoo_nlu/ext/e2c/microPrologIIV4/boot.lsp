; Mini-PrologII
; boot.lsp
;

(defun mini-PrologII ()			; to run it
  (banner)
  (format t "~A~%" (load "mlg.Start"))
  (myloop (read_prompt)))

(defun read_prompt () 
  (terpri)
  (format t "| ?- ")
  (read_code_tail))

(defun banner ()
  (dotimes (i 2) (terpri))
  (format t "Mini-PrologII~%")
  (dotimes (i 2) (terpri)))

(defun l ()
  (format t "Back to Mini-PrologII top-level~%")
  (myloop (read_prompt)))
