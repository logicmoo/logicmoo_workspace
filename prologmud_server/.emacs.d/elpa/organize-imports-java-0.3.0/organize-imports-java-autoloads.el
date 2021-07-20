;;; organize-imports-java-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "organize-imports-java" "organize-imports-java.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from organize-imports-java.el

(autoload 'organize-imports-java-erase-cache-file "organize-imports-java" "\
Clean all the buffer in the cache files." t nil)

(autoload 'organize-imports-java-reload-paths "organize-imports-java" "\
Reload the Java include paths and local source path once." t nil)

(autoload 'organize-imports-java-reload-jar-lib-paths "organize-imports-java" "\
Reload external Java paths.
For .jar files." t nil)

(autoload 'organize-imports-java-reload-local-source-paths "organize-imports-java" "\
Reload internal Java paths.
Usually Java files under project root 'src' directory." t nil)

(autoload 'organize-imports-java-clear-all-imports "organize-imports-java" "\
Clear all imports in the current buffer." t nil)

(autoload 'organize-imports-java-do-imports "organize-imports-java" "\
Do the functionalitiies of how organize imports work." t nil)

(register-definition-prefixes "organize-imports-java" '("organize-imports-java-"))

;;;***

;;;### (autoloads nil nil ("organize-imports-java-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; organize-imports-java-autoloads.el ends here
