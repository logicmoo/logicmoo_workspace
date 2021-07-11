;;; enwc-test.el --- Tests for ENWC

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: external, network, wicd, manager, nm
;; Version: 2.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://savannah.nongnu.org/p/enwc

;;; Commentary:

;;; Code:

(require 'enwc)
(require 'ert)

;; Test enwc-setup

(ert-deftest enwc-test-select-interface-save ()
  ;; Test saving the interfaces and using the ip selection back-end
  (let ((enwc-interface-list-function 'enwc--ip-interface-list)
        (enwc-ask-to-save-interfaces t)
        ;; Temporarily save our changes to a different file
        (custom-file (make-temp-file "enwc-custom-"))
        (enwc-wired-device "")
        (enwc-wireless-device ""))
    (enwc--setup-select-interfaces)
    (should (not (string-empty-p enwc-wired-device)))
    (should (not (string-empty-p enwc-wireless-device)))
    ;; Test whether the interfaces were saved
    (let ((enwc-wired-device "")
          (enwc-wireless-device ""))
      (load custom-file)
      (should (not (string-empty-p enwc-wired-device)))
      (should (not (string-empty-p enwc-wireless-device))))
    ;; Delete our temporary custom file
    (delete-file custom-file)))

(ert-deftest enwc-test-select-interface-no-save ()
  ;; Test not saving the interfaces and using the ifconfig selection back-end
  (let ((enwc-interface-list-function 'enwc--ifconfig-interface-list)
        (enwc-ask-to-save-interfaces nil)
        ;; Temporarily save our "changes" to a different file
        (custom-file (make-temp-file "enwc-custom-"))
        (enwc-wired-device "")
        (enwc-wireless-device ""))
    (enwc--setup-select-interfaces)
    (should (not (string-empty-p enwc-wired-device)))
    (should (not (string-empty-p enwc-wireless-device)))
    ;; Test whether the interfaces were saved
    (let ((enwc-wired-device "")
          (enwc-wireless-device ""))
      (load custom-file)
      (should (string-empty-p enwc-wired-device))
      (should (string-empty-p enwc-wireless-device)))
    ;; Delete our temporary custom file
    (delete-file custom-file)))

(provide 'enwc-test)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; enwc-test.el ends here
