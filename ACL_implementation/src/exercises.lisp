; given a tensor, returns a scalar with the number of elements of the tensor
(defun tally (tensor)
	(funcall (fold #'.+) (.not (.* (s 0) tensor))))

;given a tensor, returns a scalar with the number of dimensions of the tensor
(defun rank (tensor)
	(funcall (fold #'.+) (.not (.* (s 0) (shape tensor)))))

; Define the function within that, given a vector of numbers v and two
; numbers n1 and n2, returns a vector containing only the elements of v
; that are in the range between n1 and n2

(defun within (tensor n1 n2))


; given a tensor, returns a vector containing all the elements of the tensor
(defun ravel (tensor))

;given a scalar, returns a vector with all prime numbers from 2 up to the scalar, inclusive
(defun prime (n1))
	;(labels ((primep (n)
	;		(cond ((= n 3) t)
	;			  ((evenp n) nil)
	;			   t
;
;
;		)))