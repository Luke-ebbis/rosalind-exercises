; DNA object
(defclass dna ()
  ((sequence :reader dna-sequence
            :initarg :seq))
  (:documentation "Describes a sequence of DNA."))

(defmethod print-object ((obj dna) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (dna-sequence obj))))

(print
 (make-instance 'dna
  :seq "AAATTGAG"))
