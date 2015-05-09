;;;; acl_monadic_functions.lisp
;;;;
;;;; Defines APL like monadic functions.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


; given a tensor, returns a scalar with the number of elements of the tensor
(defun tally (tensor)
	(funcall (fold #'.*) (shape tensor)))

;given a tensor, returns a scalar with the number of dimensions of the tensor
(defun rank (tensor)
	(funcall (fold #'.+) (.not (.* (s 0) (shape tensor)))))

; Define the function within that, given a vector of numbers v and two
; numbers n1 and n2, returns a vector containing only the elements of v
; that are in the range between n1 and n2

(defun within (tensor n1 n2)
	(select (.and (.>= tensor (s n1)) (.<= tensor (s n2))) tensor))


; given a tensor, returns a vector containing all the elements of the tensor
(defun ravel (tensor)
	(reshape (v (tally tensor)) tensor))

;given a scalar, returns a vector with all prime numbers from 2 up to the scalar, inclusive
(defun prime (n1)
	(let* ((values (drop (s -1) (drop (s 1) (interval n1))))
		   (divisions (funcall (outer-product #.//) values values)))
		(ravel (select (.= (s 0) divisions) divisions))))
