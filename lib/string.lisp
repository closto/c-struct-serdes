(defpackage #:css.string
  (:use #:cl)
  (:export #:shrink-string
           #:join-with-last-altered))

(in-package #:css.string)

(defun shrink-string (s max-len &optional (ellipsis "..."))
  (if (> (length s) max-len)
      (let* ((trunc-length (- max-len (length ellipsis)))
             (endpos (max trunc-length 0)))
        (str:concat (subseq s 0 endpos) ellipsis))
      s))

(defun join-with-last-altered (sep strings alter-last)
  (with-output-to-string (stream)
    (loop :for (s . tail) :on strings
          :if tail
            :do (write-string s stream)
                (write-string sep stream)
          :else
            :do (write-string alter-last stream))))

(defun strjoin-horiz-with-limit (strings sep total-max each-max)
  "As long as the length of the resulting string is less than or equal to
TOTAL-MAX, the STRINGS continue to be joined. If a string is longer than
EACH-MAX, it is truncated to fit within EACH-MAX.

It returns the joined string

Example:

(css::strjoin-horiz-with-limit
   '(\"nav\" \"tally\" \"dost\" \"yem\" \"rizz\")
   \",\"
  18
  4)
;; => \"nav,t...,dost,yem\""
  (check-type strings list)
  (check-type sep string)
  (check-type total-max integer)
  (check-type each-max integer)
  (loop
    :for (s . rest) :on strings 
    :for item := (shrink-string s each-max)
      :then (shrink-string s each-max)
    :for total-length := (length item)
      :then (+ total-length (length sep) (length item))
      :and prev-total-length := (or total-length 0)
    :if (<= total-length total-max)
      :collect item :into items
    :else :if (<= (+ prev-total-length 4) total-max) :do
      (return (str:concat (str:join sep items) sep "..."))
    :else :do
      (return (join-with-last-altered sep items "..."))
    :finally 
       (return (str:join sep items))))

(defun join-with-last-append (sep strings append-last)
  (with-output-to-string (stream)
      (loop :for (s . tail) :on strings
            :do (write-string s)
                (write-string sep)
            :finally
               (write-string append-last))))

(defun strjoin-vert-with-limit (strings indent-level max-lines max-len
                                &optional (indent-string " "))
  (loop :for (s . tail) :on strings
        :for item := (shrink-string s max-len)
          :then (shrink-string s max-len)
        :for lines-cur :from 1 :to max-lines
        :collect item :into items
        :when (= lines-cur max-lines) :do
           (let* ((indent (str:repeat indent-level indent-string))
                  (newline-indent (format nil "~%~A" indent)))
             (if (print tail)
                 (return
                   (join-with-last-altered newline-indent items "..."))
                 (return
                   (str:join newline-indent items))))))
