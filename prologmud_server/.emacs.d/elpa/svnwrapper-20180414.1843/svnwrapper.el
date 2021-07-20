;;; svnwrapper.el --- Highlighting and paging for shell command `svn'

;; Copyright (C) 2018  Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.0
;; URL: https://github.com/Lindydancer/svnwrapper
;; Keywords: faces
;; Package-Requires: ((e2ansi "0.1.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The script `bin/svnwrapper.rb' is a wrapper for the command line
;; tool `svn' that adds highlighting and paging.
;;
;; What has this got to do with Emacs?  This package use Emacs (in
;; batch mode) to perform highlighting; with the help of the package
;; `e2ansi' the result is rendered using ANSI-sequences which the
;; terminal can display.
;;
;; This package highlights the output of administrative subcommand like
;; `svn status' and `svn diff'.  In addition, it highlights source
;; files viewed using `svn cat'.  It supports all programming languages
;; Emacs support, either natively or by using third party packages.

;; Requirements:
;;
;; This package requires `ruby', `emacs', and `svn' to be installed.
;; In addition, it relies in the Emacs package `e2ansi'.

;; Installation:
;;
;; Configuring `e2ansi':
;;
;; First, `e2ansi' must be configured.  This is done by setting
;; environment variables recognized by `less', typically in a suitable
;; init file.  For example (using bash syntax):
;;
;;     export "LESSOPEN=||-/PATH/TO/emacs --batch -Q -l ~/.emacs
;;                         -l e2ansi-silent -l bin/e2ansi-cat %s"
;;     export "LESS=-r -j20"
;;
;; See the documentation of `e2ansi' for more information.
;;
;; Configuring `svnwrapper.rb':
;;
;; Add an alias to be used to run `svnwrapper.rb'.  Of course, you can
;; pick `svn'.
;;
;;     alias svn="ruby ~/PATH/TO/e2ansi/bin/svnwrapper.rb -E -X ----"
;;
;; In the examples, replace "/PATH/TO" with the real paths.
;;
;; If you have installed this package through the Emacs package
;; manager, the path would look something like
;; `~/.emacs.d/elpa/svnwrapper-20180101.100/bin'.
;;
;; Installing `svnwrapper.el':
;;
;; Install the Emacs module `svnwrapper.el' using the Emacs package
;; system.
;;
;; Adapting you Emacs init file to batch mode:
;;
;; If you (like me) have a huge Emacs init file, you might need to
;; reexamine it, to make sure that it works smoothly in batch mode.
;;
;; * Make sure you don't refer to functions or variables that aren't
;;   avaiable in batch mode.  (You can use `fboundp' or `boundp' to
;;   check if a function or variable is present, respectively.)
;;
;; * Make sure your init file is quiet.  (The `e2ansi' package
;;   contains the module `e2ansi-silence.el' that can silence output
;;   from `message' and `load'.)
;;
;; * Avoid loading things that take a lot of time, that you don't need
;;   in batch mode.
;;
;; You can use the Emacs varibale `noninteractive' to conditinally run
;; code.  For example:
;;
;;     (unless noninteractive
;;       (do-something-that-would-be-pointless-in-batch-mode))
;;
;; Alternatively, you could have a simpler init file when running
;; Emacs in batch mode, say `.my-batch-emacs', and modify the
;; environment variable `LESSOPEN' accordingly.

;; The `svnwrapper.rb' utility:
;;
;; The core of this package is the file `svnwrapper.rb', written in Ruby.
;;
;;     svnwrapper [args-to-svnwrapper] [args-to-less ... ----] args-to-svn ...
;;
;; The following options can be specified:
;;
;; * `--forward-slashes' Make 'svn status' and 'svn update' output
;;   forward slashes (useful under Windows).
;;
;; * `--svn-command CMD' Use CMD when running svn, defaults to "svn".
;;
;; * `----' is used as a separator between arguments passed to `less'
;;   and arguments passed to `svn'.

;; Limitations:
;;
;; The `e2ansi' package emits highlighted output once it has read the
;; full input.  For this reason, the output of "svn update" is not
;; piped through `less'.

;;; Code:

;; ------------------------------------------------------------
;; Svn status mode
;;

(defvar svnwrapper-svn-status-font-lock-keywords
  `((,(concat
       ;; 1 - 8
       "^\\(.\\)\\(.\\)\\(.\\)\\(.\\)\\(.\\)\\(.\\)\\(.\\)\\(.\\)"
       ;; 9 - 11
       "\\(\\(\\*\\)? +\\([0-9]*\\) *\\)?"
       ;; 12
       "\\(.*\\)$")
     (12 (cond ((member (match-string-no-properties 1)
                        '("~" "!" "C" "D" "R"))
                'font-lock-warning-face)
               ((member (match-string-no-properties 1) '("A"))
                'font-lock-type-face)
               ((member (match-string-no-properties 1) '("M"))
                'font-lock-function-name-face)
               ((match-string 10)
                'font-lock-variable-name-face)
               (t
                nil))))))


;;;###autoload
(define-derived-mode svnwrapper-svn-status-mode text-mode "SvnStatus"
  "Major mode for the output of 'svn status'."
  (setq font-lock-defaults '(svnwrapper-svn-status-font-lock-keywords nil)))


;;;###autoload(add-to-list 'auto-mode-alist
;;;###autoload  '("\\.svnstatus\\'" . svnwrapper-svn-status-mode))


;; ------------------------------------------------------------
;; The end
;;

(provide 'svnwrapper)

;;; svnwrapper.el ends here
