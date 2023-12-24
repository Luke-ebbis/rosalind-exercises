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

(print (parse-file-as-string file-to-read))
