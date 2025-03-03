(in-package #:css.types)

(defun make-type-alias (base-type name)
  (check-type base-type c-type)
  (check-type name string)
  (assert (not (str:blank? name)) (name) "Suspicious empty typedef")
  (let ((typedef (make-instance 'type-alias)))
    (setf (slot-value typedef 'base-type) base-type
          (slot-value typedef 'name) name)
    typedef))

(defmethod c-type-string ((type type-alias))
  (take-slot type 'name))

(defmethod print-object ((type type-alias) stream)
  (print-unreadable-object (type stream :type t :identity t)
    (with-slots (base-type name) type
      (format stream "~a(a.k.a. ~a)"
              name (c-type-string base-type)))))

(defmethod c-type-var-assignable-p ((type type-alias))
  (with-slots (base-type) type
    (c-type-var-assignable-p base-type)))
