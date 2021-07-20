;;; system-packages-test.el --- ERT testing framework for system-packages.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: J. Alexander Branham <alex.branham@gmail.com>

(require 'ert)
(require 'system-packages)

(ert-deftest system-packages-get-install ()
  "Return correct installation command."
  (should (string=
           (let ((system-packages-use-sudo nil)
                 (system-packages-package-manager 'guix))
             (system-packages-get-command 'install))
           "guix package -i "))
  (should (string=
           (let ((system-packages-use-sudo nil)
                 (system-packages-package-manager 'pacman))
             (system-packages-get-command 'install))
           "pacman -S ")))

(ert-deftest system-packages-get-install-noconfirm ()
  "Return correct installation command."
  (should (string=
           (let ((system-packages-noconfirm t)
                 (system-packages-use-sudo nil)
                 (system-packages-package-manager 'guix))
             (system-packages-get-command 'install))
           "guix package -i "))
  (should (string=
           (let ((system-packages-noconfirm t)
                 (system-packages-use-sudo nil)
                 (system-packages-package-manager 'pacman))
             (system-packages-get-command 'install))
           "pacman -S --noconfirm"))
  (should (string=
           (let ((system-packages-noconfirm t)
                 (system-packages-use-sudo nil)
                 (system-packages-package-manager 'apt))
             (system-packages-get-command 'install "rg"))
           "apt-get install rg -y")))

(ert-deftest system-packages-errors ()
  "Error when we don't know a command."
  (should-error
   (let ((system-packages-package-manager 'pacaur))
     (system-packages-get-command 'install))))

;;; system-packages-test.el ends here
