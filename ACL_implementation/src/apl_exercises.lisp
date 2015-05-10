;;;; apl_exercises.lisp
;;;;
;;;; Defines the exercises described in the project's requirements.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


(defun tally (tensor)
	"Returns a scalar with the number of elements of the given tensor."
	(let ((non-scalar-tensor (funcall (outer-product #'.*) (v 0 0) tensor)))
		(./ (funcall (fold #'.*) (shape non-scalar-tensor)) (s 2))))

(defun rank (tensor)
	"Returns a scalar with the number of dimensions of the tensor."
	(let ((non-scalar-tensor (funcall (outer-product #'.*) (v 0 0) tensor)))
		(.- (funcall (fold #'.+) (.not (.* (s 0) (shape non-scalar-tensor)))) (s 1))))

(defun within (tensor n1 n2)
	"Returns a vector, containing only the elements of the given tensor,
	 that are in the range of the provided n1 and n2."
	(select (.and (.>= tensor (s n1)) (.<= tensor (s n2))) tensor))

(defun ravel (tensor)
	"Returns a vector containing all the elements of the tensor."
	(reshape (v (tally tensor)) tensor))

(defun prime (n1)
	"Returns a vector with all prime numbers from 2 up to the scalar, inclusive."
	(let* ((r (drop (s 1) (interval n1)))
		   (r-outer-product-r (funcall (outer-product #'.*) r r)))
		(select (.not (member? r r-outer-product-r)) r)))
