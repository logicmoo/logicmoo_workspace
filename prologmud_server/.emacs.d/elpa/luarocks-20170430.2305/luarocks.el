;;; luarocks.el --- luarocks tools                    -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/luarocks.el
;; Package-Version: 20170430.2305
;; Package-Commit: cee27ba0716edf338077387969883226dd2b7484
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; [LuaRocks][] tools.
;;
;; [LuaRocks]: https://luarocks.org/ "LuaRocks is a package manager for Lua modules."

;;; Code:
(require 'cl-lib)

(defgroup luarocks nil
  "LuaRocks tools."
  :prefix "luarocks-"
  :group 'tools)

(defcustom luarocks-executable "luarocks"
  "Path to LuaRocks executable."
  :type 'string
  :group 'luarocks)

(defun luarocks-exec-string (&rest args)
  "Call `luarocks-executable' with ARGS, returning the first line of its output."
  (with-temp-buffer
    (let ((exit-code (apply #'process-file luarocks-executable nil t nil args)))
      (if (zerop exit-code)
          (unless (bobp)
            (goto-char (point-min))
            (buffer-substring-no-properties (point) (line-end-position)))
        (error "LuaRocks command failed with exit code %S and output: %s" exit-code (buffer-string))))))

;;;###autoload
(defun luarocks-init ()
  "Initialize LuaRocks."
  (cl-assert (executable-find luarocks-executable) nil "LuaRocks executable not found: %s" luarocks-executable)
  (setenv "LUA_PATH" (luarocks-exec-string "path" "--lr-path"))
  (setenv "LUA_CPATH" (luarocks-exec-string "path" "--lr-cpath"))
  (let ((binpaths (parse-colon-path (getenv "PATH"))))
    (dolist (path (parse-colon-path (luarocks-exec-string "path" "--lr-bin")))
      (or (member path binpaths)
          (setenv "PATH" (concat path path-separator (getenv "PATH"))))
      (add-to-list 'exec-path path))))

(provide 'luarocks)
;;; luarocks.el ends here
