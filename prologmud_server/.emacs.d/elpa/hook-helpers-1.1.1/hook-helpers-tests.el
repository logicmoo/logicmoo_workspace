;;; hook-helpers-tests.el --- Tests for hook helpers

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: development, hooks
;; URL: https://savannah.nongnu.org/projects/hook-helpers-el/
;; Version: 1.1

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'hook-helpers)
(require 'ert)

(ert-deftest hkhlp-normalize-hook-spec-test ()
  (should (equal (hkhlp-normalize-hook-spec 'c++-mode-hook)
                 '((c++-mode-hook))))
  (should (equal (hkhlp-normalize-hook-spec '(c++-mode-hook . t))
                 '((c++-mode-hook . t))))
  (should (equal (hkhlp-normalize-hook-spec '(c++-mode-hook org-mode-hook))
                 '((c++-mode-hook) (org-mode-hook))))
  (should (equal (hkhlp-normalize-hook-spec '((c++-mode-hook . t) org-mode-hook))
                 '((c++-mode-hook . t) (org-mode-hook))))
  (should (equal (hkhlp-normalize-hook-spec '(c++-mode-hook (org-mode-hook . t)))
                 '((c++-mode-hook) (org-mode-hook . t))))
  (should (equal (hkhlp-normalize-hook-spec '((c++-mode-hook . t)
                                              (org-mode-hook . t)))
                 '((c++-mode-hook . t) (org-mode-hook . t))))
  (should (equal (hkhlp-normalize-hook-spec '((c++-mode-hook . t)))
                 '((c++-mode-hook . t)))))

(ert-deftest hkhlp-create-hook-helper-test ()
  (let ((test-hook nil))
    (create-hook-helper test-helper ()
      "Test Hook Helper"
      :hooks (test-hook)
      (message "This is a test"))
    (should (equal test-hook (list
                              (lambda () "Test Hook Helper" (message "This is a test")))))))

(ert-deftest hkhlp-add-hook-helper ()
  (let ((test-hook nil)
        (test-2-hook nil))
    (create-hook-helper test-helper ()
      "Test Hook Helper"
      :hooks (test-hook)
      (message "This is a test"))
    (should (equal test-hook (list
                              (lambda () "Test Hook Helper" (message "This is a test")))))
    (add-hook-helper 'test-helper '(test-2-hook))
    (should (equal test-2-hook test-hook))))

(ert-deftest hkhlp-remove-hook-helper ()
  (let ((test-hook nil))
    (create-hook-helper test-helper ()
      "Test Hook Helper"
      :hooks (test-hook)
      (message "This is a test"))
    (should (equal test-hook (list
                              (lambda () "Test Hook Helper" (message "This is a test")))))
    (remove-hook-helper 'test-helper 'test-hook)
    (should (equal test-hook nil))))

(ert-deftest hkhlp-update-hook-helper ()
  (let ((test-hook nil)
        (old-func (lambda () "Test Hook Helper" (message "This is a test")))
        (new-func (lambda () "Test Hook Helper" (message "This is another test"))))
    (create-hook-helper test-helper ()
      "Test Hook Helper"
      :hooks (test-hook)
      (message "This is a test"))
    (should (equal test-hook (list old-func)))
    (create-hook-helper test-helper ()
      "Test Hook Helper"
      :hooks (test-hook)
      (message "This is another test"))
    (should (equal test-hook (list new-func)))))

;;; hook-helpers-tests.el ends here
