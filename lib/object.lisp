(defpackage #:css.object
  (:use #:cl)
  (:export #:take-slot
           #:slot-extractor
           #:member-is-not-type-of
           #:check-member-is-type-of))
(in-package #:css.object)

(defun take-slot (obj slot &optional (fallback "<unbound>"))
  (check-type slot symbol)
  (assert (slot-exists-p obj slot))
  (if (slot-boundp obj slot)
      (slot-value obj slot)
      fallback))

(defun slot-extractor (slot &optional fallback)
  (check-type slot symbol)
  (lambda (obj)
    (take-slot obj slot fallback)))

(define-condition member-is-not-type-of (error)
  ((item :initarg :item)
   (items :initarg :items)
   (idx :initarg :idx)
   (type :initarg :type)))

(defmethod print-object ((c member-is-not-type-of) stream)
  (print-unreadable-object (c stream)
    (format stream "type error: In items=~a~&~tItem in index[~a] (which is ~a) is not type of ~a"
            (take-slot c 'items)
            (take-slot c 'idx)
            (take-slot c 'item)
            (take-slot c 'type))))

(defun check-member-is-type-of (items type &optional (every nil))
  (loop :with items-to-check := (or (and every items) (list (first items)))
        :for a-member :in items-to-check
        :for idx :from 0
        :unless (typep a-member type)
          :do (error 'member-is-not-type-of
                     :items items :idx idx :item a-member :type type)))
