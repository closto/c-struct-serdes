(defpackage #:css.tests/types
  (:use #:cl #:fiveam #:css.tests #:css.types #:rtl-user))
(in-package #:css.tests/types)

(named-readtables:in-readtable rutils:rutils-readtable)

(def-suite* types-tests
  :description "Test types"
  :in all-tests)

(test primitive-representation
  (let* ((int-t (make-primitive-type "int"))
         (var-repr (decorate-name-with-type int-t "var")))
    (is (string= "int var" var-repr))))

(test enum-print-object
  (let* ((items (str:split " " "mon dost tally yewtou ziams"))
         (constant-names (mapcar #'str:upcase items))
         (constants (mapcar #'make-enum-constant constant-names))
         (enum-t (make-enum-type constants "MY_ENUM_T"))
         (enum-repr (with-output-to-string (stream)
                      (print-object enum-t stream))))
    (is (str:contains? "MY_ENUM_T" enum-repr))
    (is (str:contains?   "MON"     enum-repr))
    (is (str:contains?   "DOST"    enum-repr))
    (is (str:contains?   "TALLY"   enum-repr))
    (is (str:contains?   "YEWTOU"  enum-repr))
    (is (str:contains?   "ZIAMS"   enum-repr))))
