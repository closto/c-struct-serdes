(in-package #:css.types)

(defmethod c-type-string ((array-t array-type))
  (with-slots (base-type shape) array-t
    (format nil "~a[~{~a~^][~}]"
            (or (c-type-string base-type)
                "<undefined>")
            shape)))

(defmethod print-object ((array-t array-type) stream)
  (print-unreadable-object (array-t stream :type t :identity t)
    (write-string (c-type-string array-t) stream)))

(defun make-array-type (base-type array-shape)
  (check-type base-type (or primitive-type enum-type struct-type)
              "one of supported types")
  (let ((array-t (make-instance 'array-type)))
    (setf (slot-value array-t 'base-type) base-type
          (slot-value array-t 'shape) array-shape)
    array-t))

(defmethod decorate-name-with-type ((array-t array-type) name)
  (let ((base-type (take-slot array-t 'base-type))
        (name-with-shape 
          (format nil "~a[~{~a~^][~}]"
                  name (take-slot array-t 'shape nil))))
    (decorate-name-with-type base-type name-with-shape)))
