;;; javaimp-tests.el --- javaimp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2019  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp-maven)

(ert-deftest javaimp-test--maven-projects-from-xml--project ()
  (with-temp-buffer
    (insert "<project/>")
    (let ((projects (javaimp--maven-projects-from-xml
		     (xml-parse-region (point-min) (point-max)))))
      (should (eql (length projects) 1)))))

(ert-deftest javaimp-test--maven-projects-from-xml--projects ()
  (with-temp-buffer
    (insert "<projects><project/><project/></projects>")
    (let ((projects (javaimp--maven-projects-from-xml
		     (xml-parse-region (point-min) (point-max)))))
      (should (eql (length projects) 2)))))
