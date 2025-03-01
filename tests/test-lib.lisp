(defpackage #:css.tests/lib
  (:use #:cl #:fiveam #:rtl-user
        #:css.tests #:css.object #:css.string))
(in-package #:css.tests/lib)

(named-readtables:in-readtable rutils:rutils-readtable)

(def-suite lib-tests
  :description "Test lib component"
  :in all-tests)

;;; Object
(def-suite* lib-object
  :description "Test for primitive-types"
  :in lib-tests)

(test check-member-type-on-nil-is-always-success
  (handler-case (check-member-is-type-of nil 'integer t)
    (t () (fail))))

(test check-member-type-tests-only-one-item-for-default-every-param
  (handler-case (check-member-is-type-of '(1 "2" "3") 'integer)
    (t () (fail))))

(test check-member-type-tests-every-item-when-every-arg-is-true
  (unless  
      (handler-case (check-member-is-type-of '(1 2 "3") 'integer t)
        (member-is-not-type-of () t)
        (t () nil))
    (fail "you failed")))
