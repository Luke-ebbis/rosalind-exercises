#!/usr/bin/env scheme-script
(import (rnrs))

;; reading in command line arguments.

(define arguments (command-line))

(define (file->list-of-chars file)
  (with-input-from-file file
    (lambda ()
      (let reading ((chars '()))
        (let ((char (read-char)))
          (if (eof-object? char)
              (reverse chars)
              (reading (cons char chars))))))))
(define data (
  file->list-of-chars (list-ref arguments 1)))
(display data)

(newline)
