(defclass book ()
  ((title :reader book-title
          :initarg :title)
   (author :reader book-author
           :initarg :author))
  (:documentation "Describes a book."))
(defmethod print-object ((obj book) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (book-title obj))))

(print (make-instance 'book
               :title "ANSI Common Lisp"
               :author "Paul Graham"))
