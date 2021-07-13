;; Start From https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

(defun ensure-package-installed (&rest packages)
   "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
 (mapcar
  (lambda (package)
   ;; (package-installed-p 'evil)
   (if (package-installed-p package)
    nil
    (if (y-or-n-p (format "Package %s is missing. Install it? " package))
     (package-install package)
     package)))
  packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
 (package-refresh-contents))

;; (ensure-package-installed 'iedit 'magit) ;  --> (nil nil) if iedit and magit are already installed
;; 
;; ;; activate installed packages
;; (package-initialize)

;; End From https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
