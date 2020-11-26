(provide 'show)

(save-excursion 
  (set-buffer (get-buffer-create "prolog-database"))
  (newline))
  

(defun show-database ()
  ; this function makes the database buffer visable according to the
  ; user's preferences.
  (interactive)
  ; what to do?
  )

(defun show-assertion (p) 
  (save-excursion 
    (set-buffer "prolog-database")
    (end-of-buffer)
    (insert p)
    (newline)))


(defun hide-assertion  (p) 
  (save-excursion 
    (set-buffer "prolog-database")
    (beginning-of-buffer)
    (search-forward (concat "\n"  p "\n"))
    (previous-line 1)
    (beginning-of-line)
    (kill-line 1)))

