#!/usr/bin/env scheme-script
(import (rnrs)
        (rnrs hashtables (6))         ; for R6RS hash tables
        (rnrs base (6)))              ; for R6RS let-values
;; reading in command line arguments.

;; (define arguments (command-line))

;; Non interactive mode chars.
(define arguments (list "script" "/home/sibbe/Documents/private/projects/programming-projects/challenges/rosalind-exercises/dna/dna_test.txt"))


;; Here we read in the data from a file.
(define (file->list-of-chars file)
  (with-input-from-file file
    (lambda ()
      (let reading ((chars '()))
        (let ((char (read-char)))
          (if (eof-object? char)
              (reverse chars)
              (reading (cons char chars))))))))

(define (hashtable->alist ht)
  (let-values (((ks vs) (hashtable-entries ht)))
    (vector->list (vector-map cons ks vs))))

;; reading in the data
(define data (
  file->list-of-chars (list-ref arguments 1)))

(define hashmap (
	hashtable->alist (list-ref data)))

(display hashmap)






(newline)
