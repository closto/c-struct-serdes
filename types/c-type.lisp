(in-package #:css.types)

(defparameter *anonymous-print* "<unnamed>")

(defclass c-type ()
  ())

(defgeneric c-type-string (c-type)
  (:documentation "
Pretty print representation of c-type.

In most cases, `c-type' is just the same as the slot value of `name'. However,
when it comes to arrays, it should be \"T[]\", and for enums, it should be
\"enum T\".
"))

(defgeneric decorate-name-with-type (c-type name)
  (:documentation "
Decorate the given name with the given type, i.e. make a variable declaration
string.

If the given c-type is `int' and the name is `var1', the produced string will be
\"int var1\". In another example, if the given c-type is `int[3][4]' and the
name is `arr' , then the produced string will be \"int arr[3][4]\".
"))

(defgeneric c-type-var-assignable-p (c-type)
  (:documentation  "
Determines whether a variable of the given type can be assigned from another
variable.

This is true for most types, but assignment is not allowed for the array type.
Also, for the char-family pointer type, assigning is not permitted in the
string semantics (that is, assigning a string literal to a char-family pointer
variable).
"))

(defmethod c-type-string ((type c-type))
  "<undefined>")

(defmethod decorate-name-with-type ((type c-type) name)
  (str:join " " (list (c-type-string type) name)))

(defmethod c-type-var-assignable-p ((type c-type))
  t)

;;; Classes
;; Primitive Type
(defclass primitive-type (c-type)
  ((name :accessor name)))

;; Enum Constant (member of enum-type)
(defclass enum-constant ()
  ((name :accessor name)
   (value :accessor constant-value)))

;; Enum Type
(defclass enum-type (c-type)
  ((name :accessor name)
   (constants :accessor enum-constants)))

;; Array Type
(defclass array-type (c-type)
  ((base-type :accessor base-type)
   (shape :accessor array-shape)))

;; Struct Member (member of struct-type)
(defclass struct-member ()
  ((name :accessor name)
   (base-type :accessor base-type)
   (bitfield :accessor struct-member-bitfield)))

;; Struct Type
(defclass struct-type (c-type)
  ((name :accessor name)
   (members :accessor struct-members)))

