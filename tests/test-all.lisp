(defpackage #:css.tests
  (:use #:cl #:fiveam)
  (:export #:run!
           #:all-tests))

(in-package #:css.tests)

(def-suite all-tests
  :description "Test all")
