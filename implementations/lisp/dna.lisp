#!/usr/bin/env -S sbcl --dynamic-space-size 9000 --script
; DNA object
(defclass dna ()
  ((sequence :reader dna-sequence
            :initarg :seq
            :initform (error "You did not supply a dna sequence.")))
  (:documentation "Describes a sequence of DNA."))


; representation
(defmethod print-object ((obj dna) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (dna-sequence obj))))

(print
 (make-instance 'dna
  :seq "AAATTGAG"))
