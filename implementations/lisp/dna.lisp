#!/usr/bin/env -S sbcl --dynamic-space-size 9000 --script
;; command line argument
(defvar file-to-read (nth 1 sb-ext:*posix-argv*))


(defun parse-file-as-string (file-path)
  "Read the contents of the file at the specified path and return it as a string."
  (with-open-file (stream file-path
                          :direction :input
                          :element-type 'character
                          :if-does-not-exist nil)
    (if stream
        (let ((contents (make-string (file-length stream))))
          (read-sequence contents stream)
          contents)
        (error "Failed to open file: ~A" file-path))))

(defun break-to-list (str)
	(coerce str 'list))

;;; https://stackoverflow.com/a/59079382/15753558 recursive.
(defun remove-element (symbol list replaced)
  (if (null list)
      (reverse replaced)
    (if (eql (car list) symbol)
        (remove-element symbol (cdr list) (cons nil         replaced)) 
        (remove-element symbol (cdr list) (cons (car list) replaced)))))

(defun set-from-list (lst)
    (let ((st))
        (loop for ele in lst
            do
            (if (not (member ele st))
                (push ele st))) st))
(defun count-in-list (lst)
		(let ((vec '()))
		(let ((st (set-from-list lst)))
				(loop for item in st
						do
						(push (count item lst) vec )))
		(nreverse vec)))


(defvar input-data 
(remove nil	(remove-element '#\Newline (break-to-list (parse-file-as-string 
																							file-to-read)) () )) )
(format t "~{~a~^ ~}"  (count-in-list input-data))


