(in-package #:css.types)

;;; Struct Member (member of struct-type)
(defclass struct-member ()
  (name base-type bitfield))

(defun print-struct-member (st-memb &optional (stream *standard-output*))
  (check-type st-memb struct-member)
  (with-slots (name base-type) st-memb
        (write-string (decorate-name-with-type base-type name)
                      stream)))

(defun struct-member-string (st-memb)
  (with-output-to-string (stream)
    (print-struct-member st-memb stream)))

(defmethod print-object ((st-memb struct-member) stream)
  (print-unreadable-object (st-memb stream :type t :identity t)
    (print-struct-member st-memb stream)))

(defun make-struct-member (base-type &optional name bitfield)
  (check-type base-type c-type)
  (check-type name (or null string))
  (check-type bitfield (or null string))
  (let ((smemb (make-instance 'struct-member)))
    (setf (slot-value smemb 'base-type) base-type
          (slot-value smemb 'name) name
          (slot-value smemb 'bitfield) bitfield)
    smemb))

;;; Struct Type
(defclass struct-type (c-type)
  (name members))

(defmethod print-object ((struct-t struct-type) stream)
  (print-unreadable-object (struct-t stream :type t :identity t)
    (format stream "~a {~&~a~&}"
            (take-slot struct-t 'name)
            (strjoin-vert-with-limit (mapcar #'struct-member-string
                                             (take-slot struct-t 'members nil))
                                     4 16 60))))

(defun make-struct-type (members &optional name)
  (check-type members list)
  (check-member-is-type-of members 'struct-type)
  (let ((struct-t (make-instance 'struct-type)))
    (setf (slot-value struct-t 'name) name
          (slot-value struct-t 'members) members)
    struct-t))
