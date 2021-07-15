;;; helm-w32-launcher.el --- Start Menu entry launcher using Helm -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/helm-w32-launcher
;; Version: 0.1.6
;; Package-Requires: ((emacs "24") (helm "1.6.5") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; See license.txt for licensing information.

;;; Commentary:

;; Launch Start Menu entries using Emacs and Helm.
;; Why?
;;  * Why not?
;;  * Because Helm is superior to the Start Menu (or Start Screen) search
;;    feature.
;;
;; To use, see the following commands:
;;  * `helm-w32-launcher'
;;  * `helm-w32-launcher-elevated'
;;  * `helm-w32-launcher-open-shortcut-directory'
;;  * `helm-w32-launcher-open-shortcut-properties'

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'helm)

(defgroup helm-w32-launcher nil
  "Start Menu entry launcher."
  :group 'external
  :group 'helm)

(defcustom helm-w32-launcher-csc-executable nil
  "The C# compiler executable.
It can be either a file name or nil, in which case auto-detection is
attempted.
It's used only once, to compile the C# helper."
  :type '(choice (file :tag "Path")
                 (const :tag "Try to guess" nil))
  :group 'helm-w32-launcher)

(defcustom helm-w32-launcher-use-cache t
  "Whether to cache the Start Menu entries.
If non-nil, the default, cache them."
  :type 'boolean
  :group 'helm-w32-launcher)

(defcustom helm-w32-launcher-fuzzy-match nil
  "Whether to enable fuzzy matching in `helm-w32-launcher'.
If non-nil, enable fuzzy matching."
  :type 'boolean
  :group 'helm-w32-launcher)

(defcustom helm-w32-launcher-use-paths-for-completion nil
  "Whether to consider shortcut paths when completing candidates.
If non-nil, the pattern will be matched against full paths of shortcuts
in addition to the shortcut names."
  :type 'boolean
  :group 'helm-w32-launcher)

(defgroup helm-w32-launcher-faces nil
  "`helm-w32-launcher' faces."
  :group 'helm-w32-launcher
  :group 'helm-faces)

(defface helm-w32-launcher-path
  '((t :inherit font-lock-comment-face))
  "Face to highlight full paths of Start Menu entries."
  :group 'helm-w32-launcher-faces)

;;;###autoload
(defun helm-w32-launcher ()
  "Launch a program as if from the Start Menu.
When `helm-w32-launcher-use-cache' is non-nil, this function caches
the Start Menu entries, use `helm-w32-launcher-flush-cache' to flush
the cache."
  (interactive)
  (helm-w32-launcher--helm #'helm-w32-launcher--launch))

;;;###autoload
(defun helm-w32-launcher-elevated ()
  "Launch a program as if from the Start Menu with elevated privileges.
When `helm-w32-launcher-use-cache' is non-nil, this function caches
the Start Menu entries, use `helm-w32-launcher-flush-cache' to flush
the cache."
  (interactive)
  (helm-w32-launcher--helm #'helm-w32-launcher--launch-elevated))

;;;###autoload
(defun helm-w32-launcher-open-shortcut-directory ()
  "Open the directory of the selected shortcut.
When `helm-w32-launcher-use-cache' is non-nil, this function caches
the Start Menu entries, use `helm-w32-launcher-flush-cache' to flush
the cache."
  (interactive)
  (helm-w32-launcher--helm #'helm-w32-launcher--open-directory))

;;;###autoload
(defun helm-w32-launcher-open-shortcut-properties ()
  "Open the properties of the selected shortcut.
When `helm-w32-launcher-use-cache' is non-nil, this function caches
the Start Menu entries, use `helm-w32-launcher-flush-cache' to flush
the cache."
  (interactive)
  (helm-w32-launcher--helm #'helm-w32-launcher--open-properties))

(defvar helm-w32-launcher--entry-cache nil
  "The Start Menu entry cache, as returned by the helper program.
It's a list of (NAME . FULL-PATH-TO-LNK-FILE).")

;;;###autoload
(defun helm-w32-launcher-flush-cache ()
  "Flush the internal `helm-w32-launcher' cache."
  (interactive)
  (setq helm-w32-launcher--entry-cache nil))

(defun helm-w32-launcher--helm (action)
  "Execute the Helm source.
ACTION is the function to call upon selecting a candidate."
  (helm :buffer "*helm w32-launcher*"
        :sources
        (let ((name "W32 Launcher")
              (entries (helm-w32-launcher--get-entries)))
          (if helm-w32-launcher-use-paths-for-completion
              (helm-build-sync-source name
                :candidates (helm-w32-launcher--show-paths entries nil)
                :fuzzy-match helm-w32-launcher-fuzzy-match
                :action action)
            (helm-build-sync-source name
              :candidates entries
              :fuzzy-match helm-w32-launcher-fuzzy-match
              :action action
              :filtered-candidate-transformer
              #'helm-w32-launcher--show-paths)))))

(defun helm-w32-launcher--get-entries ()
  "Get Start Menu entries, possibly using the cache."
  (cond
   ((not helm-w32-launcher-use-cache)
    (helm-w32-launcher--call-helper-list-items))
   (helm-w32-launcher--entry-cache
    helm-w32-launcher--entry-cache)
   (t
    (setq helm-w32-launcher--entry-cache
          (helm-w32-launcher--call-helper-list-items)))))

(defun helm-w32-launcher--launch (shortcut-path)
  "Open the shortcut located at SHORTCUT-PATH."
  (helm-w32-launcher--shell-execute "open" shortcut-path))

(defun helm-w32-launcher--launch-elevated (shortcut-path)
  "Open the shortcut located at SHORTCUT-PATH with elevated privileges."
  (helm-w32-launcher--shell-execute "runas" shortcut-path))

(defun helm-w32-launcher--open-directory (shortcut-path)
  "Open the directory of SHORTCUT-PATH."
  (helm-w32-launcher--shell-execute "--explore--" shortcut-path))

(defun helm-w32-launcher--open-properties (shortcut-path)
  "Open the properites of the shortcut at SHORTCUT-PATH."
  (helm-w32-launcher--shell-execute "properties" shortcut-path))

(defun helm-w32-launcher--show-paths (candidates _source)
  "Add the full paths to the list of CANDIDATES."
  (mapcar (lambda (candidate)
            (cons (concat (car candidate)
                          (propertize (concat " [" (cdr candidate) "]")
                                      'face 'helm-w32-launcher-path))
                  (cdr candidate)))
          candidates))

(defconst helm-w32-launcher--package-directory
  (file-name-directory (or load-file-name default-directory)))
(defconst helm-w32-launcher--helper-source
  (expand-file-name "helper-src/*" helm-w32-launcher--package-directory))
(defconst helm-w32-launcher--helper-name
  (expand-file-name "helper.exe" helm-w32-launcher--package-directory))

(defun helm-w32-launcher--call-helper-list-items ()
  "Call the helper program to get the list of Start Menu items."
  (read (helm-w32-launcher--call-helper "ItemLister")))

(defun helm-w32-launcher--shell-execute (operation shortcut)
  "Get Windows to perform OPERATION on SHORTCUT.
See `w32-shell-execute' for details."
  ;; Do it via the helper, because Emacs 24.3 and earlier are unable to cope
  ;; with characters outside of the current codepage in paths because they use
  ;; legacy APIs. The helper OTOH will use the Unicode API calls.
  (helm-w32-launcher--call-helper
   "ProcessStarter"
   ;; Emacs would mess up the encoding here, too, so encode the arguments.
   (helm-w32-launcher--encode-string operation)
   (helm-w32-launcher--encode-string shortcut)))

(defun helm-w32-launcher--call-helper (&rest args)
  "Call the helper program with ARGS and return its output.."
  (condition-case nil
      (apply #'helm-w32-launcher--call-process
             helm-w32-launcher--helper-name args)
    (file-error
     ;; The helper program not found, try to compile it.
     (unless helm-w32-launcher-csc-executable
       (setq helm-w32-launcher-csc-executable
             (or (helm-w32-launcher--guess-csc-executable)
                 (error "Can't guess the path to csc.exe
Please set `helm-w32-launcher-csc-executable'"))))
     (helm-w32-launcher--call-process
      helm-w32-launcher-csc-executable
      "/nologo" "/t:exe" "/debug-" "/utf8output" "/o"
      (concat "/out:" (helm-w32-launcher--slash-to-backslash
                       helm-w32-launcher--helper-name))
      (helm-w32-launcher--slash-to-backslash helm-w32-launcher--helper-source))
     ;; Compiled successfully, try to run it again.
     (apply #'helm-w32-launcher--call-process
            helm-w32-launcher--helper-name args))))

(defun helm-w32-launcher--encode-string (string)
  "Encode a STRING by treating it as UTF-8 and base64-encoding it."
  (base64-encode-string (encode-coding-string string 'utf-8)))

(defun helm-w32-launcher--slash-to-backslash (string)
  "Return a new STRING with all slashes replaced with backslashes."
  (replace-regexp-in-string (rx "/") "\\" string t t))

(put 'helm-w32-launcher-process-returned-non-zero
     'error-conditions '(helm-w32-launcher-process-returned-non-zero error))
(put 'helm-w32-launcher-process-returned-non-zero
     'error-message "Process returned non-zero")

(defun helm-w32-launcher--call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.
PROGRAM and ARGS are as in `call-process'.
The PROGRAM's output, decoded using UTF-8, is returned as a string."
  (with-temp-buffer
    (let* ((coding-system-for-read 'utf-8)
           (error-code (apply #'call-process program nil t nil args))
           (result (buffer-substring-no-properties 1 (1+ (buffer-size)))))
      (when (/= 0 error-code)
        (signal 'helm-w32-launcher-process-returned-non-zero
                (list program error-code result)))
      result)))

(defun helm-w32-launcher--guess-csc-executable ()
  "Try to guess the path to the newest possible C# compiler executable."
  (cl-block return
    (let* ((dotnet-dir (expand-file-name "Microsoft.NET" (getenv "WINDIR")))
           (fx-parent-dir
            ;; Prefer the 64-bit framework.
            (let ((fx64-dir (expand-file-name "Framework64" dotnet-dir)))
              (if (file-directory-p fx64-dir)
                  fx64-dir
                (let ((fx32-dir (expand-file-name "Framework" dotnet-dir)))
                  (if (file-directory-p fx32-dir)
                      fx32-dir
                    (cl-return-from return nil))))))
           (installed-fx-directories
            (sort
             (delq
              nil
              (mapcar
               (lambda (name)
                 (let ((length-without-last (1- (length name))))
                   (and (= ?/ (aref name length-without-last))
                        (condition-case nil
                            ;; Add version lists to names to save some work
                            ;; further down the road.
                            (cons name (version-to-list
                                        (substring name 1 length-without-last)))
                          ;; The name is not a valid version number, skip.
                          (error nil)))))
               (file-name-all-completions "v" fx-parent-dir)))
             (lambda (a b)
               (version-list-< (cdr b) (cdr a))))))
      (dolist (installed-fx-dir installed-fx-directories)
        (let ((csc-path (expand-file-name
                         "csc.exe"
                         (expand-file-name (car installed-fx-dir)
                                           fx-parent-dir))))
          (when (file-exists-p csc-path)
            (cl-return-from return csc-path)))))
    nil))

(provide 'helm-w32-launcher)
;;; helm-w32-launcher.el ends here
