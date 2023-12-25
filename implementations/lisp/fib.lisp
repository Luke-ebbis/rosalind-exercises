#!/usr/bin/env -S sbcl --dynamic-space-size 9000 --script
(defun recursive-rabbits (n k)
	(if (>= 1 n)
   		(return-from recursive-rabbits n)
			(+ (recursive-rabbits (- n 1) k) (* (recursive-rabbits (- n 2) k) k))))

(defun parse-cli (x)
	(let* ((n (parse-integer (nth 1 sb-ext:*posix-argv*)))
				(k (parse-integer (nth 2 sb-ext:*posix-argv*))))
	(list n k)))

(defun main ()
	(let* ((arguments (parse-cli sb-ext:*posix-argv*))
				 (result (reduce #'recursive-rabbits arguments)))
		(format t "~d~%" result)))
(main)
(sb-ext:save-lisp-and-die "fib-bin" :executable t :toplevel 'main)
