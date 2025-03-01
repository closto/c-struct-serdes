(defpackage #:css.tests/types
  (:use #:cl #:fiveam #:css.tests #:css.types #:rtl-user))
(in-package #:css.tests/types)

(named-readtables:in-readtable rutils:rutils-readtable)

(def-suite types-tests
  :description "Test types"
  :in all-tests)

;;; Primitive Types
(def-suite* primitive-types
  :description "Test for primitive-types"
  :in types-tests)

(test primitive-type-has-name
  (let ((int-t (make-primitive-type "int")))
    (is (string= "int" (name int-t)))))

(test primitive-type-name-is-its-representation

  (let ((int-t (make-primitive-type "int")))
    (is (string= "int"
                 (c-type-string int-t))))

  (let ((int-t (make-primitive-type "int")))
    (is (string= "int var"
                 (decorate-name-with-type int-t "var")))))

;;; Enum Types
(def-suite* enum-types
  :description "Test for enum-types"
  :in types-tests)

(test enum-constant-have-name-and-value
  (let ((econ-good (make-enum-constant "GOOD" "1")))
    (is (string= "GOOD" (name econ-good)))
    (is (string= "1"    (constant-value econ-good)))))

(test enum-constant-can-have-empty-value
  (let ((econ-bad (make-enum-constant "BAD")))
    (is (string= "BAD" (name econ-bad)))
    (is (null          (constant-value econ-bad)))))

(test enum-type-has-name-and-enum-constants
  (let* ((econ-bad  (make-enum-constant "BAD"  "0"))
         (econ-good (make-enum-constant "GOOD" "1"))
         (enum-t    (make-enum-type (list econ-bad econ-good)
                                   "my_enum_t")))
    (is (string= "my_enum_t" (name enum-t)))

    (let* ((const-members (enum-constants enum-t))
           (const-names   (mapcar #'name const-members)))
      (is (str:s-member const-names "GOOD"))
      (is (str:s-member const-names "BAD")))))

(test enum-type-can-be-anonymous
  (let* ((econ-bad  (make-enum-constant "BAD"  "0"))
         (econ-good (make-enum-constant "GOOD" "1"))
         (enum-t    (make-enum-type (list econ-bad econ-good))))
    (is (null (name enum-t)))))

(test enum-type-can-be-anonymous-and-have-empty-constants
  (let* ((enum-t (make-enum-type nil)))
    (is (null (name enum-t)))
    (is (null (enum-constants enum-t)))))

(test enum-type-name-following-enum-keyword-is-its-representation
  (let* ((enum-t    (make-enum-type nil "my_enum_t"))
         (enum-repr (c-type-string enum-t)))
    (is (string= "enum my_enum_t" enum-repr))))

;;; Array Type
(def-suite* array-types
  :description "Test for array-types"
  :in types-tests)

(test array-has-base-type-and-shape
  (let* ((char-t  (make-primitive-type "char"))
         (shape  '("10" "MAX_BUF_LEN"))
         (array-t (make-array-type char-t shape)))
    (is (string= "char" (name (base-type array-t))))
    (is (equal '("10" "MAX_BUF_LEN") (array-shape array-t)))))

(test array-base-type-name-with-shape-is-array-type-representation
  (let* ((char-t  (make-primitive-type "char"))
         (shape  '("10" "MAX_BUF_LEN"))
         (array-t (make-array-type char-t shape))
         (array-repr (c-type-string array-t)))
    (is (string= "char[10][MAX_BUF_LEN]" array-repr))))

(test array-type-decorate-name-in-the-same-way-as-declaring-variable
  (let* ((enum-t  (make-enum-type nil "my_enum_t"))
         (shape  '("10" "N_STATS"))
         (array-t (make-array-type enum-t shape))
         (var-repr (decorate-name-with-type array-t "arr")))
    (is (string= "enum my_enum_t arr[10][N_STATS]" var-repr))))

(test array-type-can-have-any-length-of-shape
  (let* ((int-t   (make-primitive-type "int"))
         (shape  '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
         (array-t (make-array-type int-t shape))
         (array-repr (c-type-string array-t)))
    (is (string= "int[1][2][3][4][5][6][7][8][9]" array-repr))))

;;; Struct Type
(def-suite* struct-types
  :description "Test for enum-types"
  :in types-tests)

(test struct-member-has-name-and-type-and-bitfield
  (let* ((int-t   (make-primitive-type "int"))
         (st-memb (make-struct-member int-t "memb1" "BIT_N")))
    (is (string= "int"   (name (base-type st-memb))))
    (is (string= "memb1" (name st-memb)))
    (is (string= "BIT_N" (struct-member-bitfield st-memb)))))

(test struct-member-can-be-anonymous
  (let* ((int-t   (make-primitive-type "int"))
         (st-memb (make-struct-member int-t)))
    #| Struct member can be anonymous for two reasons.
    1. Bit-field member can be anonymous
       (In this scenario, its type must be one of integer types)
    2. Members of anonymous struct or union member can be embedded
       effectively into the enclosing structure.

    However, it is important to note that CSS itself is not a parser;
    therefore, it will not check the type or the enclosing structure
    if there are anonymous struct members. |#
    (is (null (name st-memb)))))

(test struct-member-representation-is-same-as-variable-declaration
  (let* ((int-t   (make-primitive-type "int"))
         (st-memb (make-struct-member int-t "memb1")))
    (is (string= "int memb1" (struct-member-string st-memb)))))

(test anonymous-struct-member-can-be-represented
  (let* ((int-t     (make-primitive-type "int"))
         (anon-memb (make-struct-member int-t)))
    (is (string= "int" (str:trim (struct-member-string anon-memb))))))

(test struct-type-has-name-and-members
  (let* ((int-t    (make-primitive-type "int"))
         (memb1    (make-struct-member int-t "memb1"))
         (float-t  (make-primitive-type "float"))
         (memb2    (make-struct-member float-t "memb2"))
         (struct-t (make-struct-type (list memb1 memb2) "my_struct_t")))
    
    (let* ((members      (struct-members struct-t))
           (member-names (mapcar #'name members)))
      (is (str:s-member member-names "memb1"))
      (is (str:s-member member-names "memb2"))
      (is (string= "my_struct_t" (name struct-t))))))

(test struct-type-can-be-anonymous
  (let* ((int-t    (make-primitive-type "int"))
         (memb1    (make-struct-member int-t "memb1"))
         (float-t  (make-primitive-type "float"))
         (memb2    (make-struct-member float-t "memb2"))
         (struct-t (make-struct-type (list memb1 memb2))))
    (is (null (name struct-t)))))

(test struct-keyword-plus-struct-type-name-is-the-struct-type-representaiton
  (let* ((struct-t    (make-struct-type nil "my_struct_t"))
         (struct-repr (c-type-string struct-t)))
    (is (string= "struct my_struct_t" struct-repr))))

(test anonymous-struct-can-be-represented-in-order-to-debug
  (let* ((struct-t    (make-struct-type nil))
         (struct-repr (c-type-string struct-t)))
    (is (string= (format nil "struct ~a" *anonymous-print*)
                 struct-repr))))
