(in-package #:css.types)

(defmethod print-object ((econ enum-constant) stream)
  (print-unreadable-object (econ stream :type t :identity t)
    (format stream "~a~@[(=~a)~]"
            (take-slot econ 'name)
            (take-slot econ 'value nil))))

(defun make-enum-constant (name &optional value)
  (check-type name string)
  (let ((econ (make-instance 'enum-constant)))
    (setf (slot-value econ 'name) name
          (slot-value econ 'value) value)
    econ))

(defmethod print-object ((enum-t enum-type) stream)
  (labels ((enum-type-name (name)
             (if name name (shrink-string name 16)))
           (enum-type-members (constants)
             (handler-case
                 (let ((constant-names
                         (mapcar (slot-extractor 'name) constants)))
                   (strjoin-horiz-with-limit constant-names "," 40 16))
               (error () constants))))
    (print-unreadable-object (enum-t stream :type t :identity t)
      (format stream "~a~@[{~a}~]"
              (enum-type-name (take-slot enum-t 'name))
              (enum-type-members (take-slot enum-t 'constants nil))))))

(defun make-enum-type (constants &optional name)
  (check-type name (or null string))
  (let ((enum-t (make-instance 'enum-type)))
    (setf (slot-value enum-t 'name) name
          (slot-value enum-t 'constants) constants)
    enum-t))

(defmethod c-type-string ((enum-t enum-type))
  (let ((enum-type-name (or (take-slot enum-t 'name) "<unnamed>")))
    (str:concat "enum " enum-type-name)))

;; no special treatment required for decorate-name-with-type
