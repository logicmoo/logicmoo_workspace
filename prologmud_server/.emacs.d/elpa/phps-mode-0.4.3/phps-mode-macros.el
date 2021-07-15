;;; phps-mode-macros.el --- Macros for major mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

;;; Commentary:


;;; Code:

(defconst
  phps-mode-macrotime-debug
  nil
  "Debug messages during macro expansion time, default nil.")

(defmacro phps-mode-debug-message (&rest message)
  "Display debug MESSAGE when debug flag is on."
  `(when ,phps-mode-macrotime-debug
    ,@message))

(provide 'phps-mode-macros)
;;; phps-mode-macros.el ends here
