#!/usr/bin/env -S sbcl --dynamic-space-size 9000 --script
;; command line argument

;;;; Parse a file contents to string.
(defun parse-file (file-path)
  "Read the contents of the file at the specified path and return as string."
  (with-open-file (stream file-path
                          :direction :input
                          :element-type 'character
                          :if-does-not-exist nil)
    (if stream
        (let ((contents (make-string (file-length stream))))
          (read-sequence contents stream)
          contents)
        (error "Failed to open file: ~A" file-path))))

;;; Make a list from a string.
(defun break-to-list (str) 
	(coerce str 'list))

;;; https://stackoverflow.com/a/59079382/15753558 recursive method to remove 
;;; elements from an list..
(defun remove-element (symbol list replaced)
  (if (null list)
      (reverse replaced)
    (if (eql (car list) symbol)
        (remove-element symbol (cdr list) (cons nil replaced)) 
        (remove-element symbol (cdr list) (cons (car list) replaced)))))

(defun set-from-list (lst)
    (let ((st))
        (loop for ele in lst
            do
            (if (not (member ele st))
                (push ele st))) st))

(defun count-in-list (lst st)
  (let ((vec '()))
    (dolist (item st)
      (push (count item lst) vec))
    (nreverse vec)))

(defun show-output (output)
	(format t "~{~a~^ ~}~%" output))

(defun main ()
	(let* ((file-to-read (nth 1 sb-ext:*posix-argv*))
				 (input (break-to-list (parse-file file-to-read)))
				 (sequence-list (remove nil 
																(remove-element '#\Newline input nil)))
				 (dna-set '(#\A #\C #\G #\T))
				 (freq (count-in-list sequence-list dna-set)))
  (show-output freq))	)

(main)

;; Writing this code to a binary...
(sb-ext:save-lisp-and-die "dna-bin" :executable t :toplevel 'main)
