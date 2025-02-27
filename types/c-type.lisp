(in-package #:css.types)

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

(defmethod c-type-string ((type c-type))
  "<undefined>")

(defmethod decorate-name-with-type ((type c-type) name)
  (str:join " " (list (c-type-string type) name)))

