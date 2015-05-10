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

(defun tally (tensor)
	"given a tensor, returns a scalar with the number of elements of the tensor. Force to be non-scalar because scalar has no dimensions"
	(./ (funcall (fold #'.*) (shape (funcall (outer-product #'.*) (v 0 0) tensor))) (s 2)))

(defun rank (tensor)
	"given a tensor, returns a scalar with the number of dimensions of the tensor"
	(funcall (fold #'.+) (.not (.* (s 0) (shape tensor)))))

(defun within (tensor n1 n2)
	"Returns a vector containing the elements of tensor withing n1 and n2"
	(select (.and (.>= tensor (s n1)) (.<= tensor (s n2))) tensor))


(defun ravel (tensor)
	"given a tensor, returns a vector containing all the elements of the tensor"
	(reshape (v (tally tensor)) tensor))

(defun prime (n1)
	"given a scalar, returns a vector with all prime numbers from 2 up to the scalar, inclusive"
	(let* ((r (drop (s 1) (interval n1)))
		   (r-outer-product-r (funcall (outer-product #'.*) r r)))
		(select (.not (member? r r-outer-product-r)) r)))