; given a tensor, returns a scalar with the number of elements of the tensor
(defun tally (tensor)
	(let ((count 0))
		(funcall (scan (lambda (x) (incf count))) tensor)
		count))

;given a tensor, returns a scalar with the number of dimensions of the tensor
(defun rank ())


; Define the function within that, given a vector of numbers v and two
; numbers n1 and n2, returns a vector containing only the elements of v
; that are in the range between n1 and n2

(defun within ())

; given a tensor, returns a vector containing all the elements of the tensor
(defun ravel ())

;given a scalar, returns a vector with all prime numbers from 2 up to the scalar, inclusive
(defun prime ())
