(defpackage #:css.tests
  (:use #:cl #:fiveam)
  (:export #:run!
           #:all-tests))

(in-package #:css.tests)

(def-suite all-tests
  :description "Test myself")

(in-suite all-tests)

(test yayayayayay
  (fiveam:pass))
