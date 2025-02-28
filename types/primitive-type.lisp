(in-package #:css.types)

(defmethod print-object ((prim-t primitive-type) stream)
  (print-unreadable-object (prim-t stream :type t :identity t)
    (format stream "~a"
            (take-slot prim-t 'name))))

(defun make-primitive-type (name)
  (check-type name string)
  (let ((prim-t (make-instance 'primitive-type)))
    (setf (slot-value prim-t 'name) name)
    prim-t))

(defmethod c-type-string ((type c-type))
  (take-slot type 'name))

;; no special treatment required for decorate-name-with-type
