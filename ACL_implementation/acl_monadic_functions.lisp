;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Internal funtions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Create a copy of the tensor given by the arguments applying the function
(defgeneric create-tensor-1 (function tensor)
	(:method ((function t) (tensor t))
		(error "Not supported")))

(defmethod create-tensor-1 (function (tensor tensor-scalar))
	(s (funcall function (tensor-content tensor))))

(defmethod create-tensor-1 (function (tensor tensor-vector))
	(let ((result (list))
		  (original-vector (tensor-content tensor)))

		(dotimes (i (array-dimension original-vector 0))
			(setf result (append result (list (funcall function (aref original-vector i))))))
		; convert list to arguments
		(apply #'v result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Creates a tensor whose elements are the symmetric of the corresponding elements of the argument tensor.
(defun .- (tensor)
	(create-tensor-1 #'simetric tensor))

;Same as the previous one, but using the inverse.
(defun ./ (tensor)
	(create-tensor-1 #'inverse tensor))

;Same as the previous one, but using the factorial.
(defun .! (tensor)
	(create-tensor-1 #'fact tensor))

;Same as the previous one, but using the sin function
(defun .sin (tensor)
	(create-tensor-1 #'sin tensor)) 

;Same as the previous one, but using the cos function.
(defun .cos (tensor)
	(create-tensor-1 #'cos tensor))

;Same as the previous one, but using the negation. The result is a tensor containing, as element, the integer 0 or 1, depending on the corresponding element in the arugment tensor being different that zero or equal to zero.
(defun .not (tensor)
	(create-tensor-1 (lambda (n) (negate (create-bool n))) tensor))
 
;Creates a vector containing the length of each dimension of the argument tensor.
(defun shape (tensor))

;Creates a vector containing an enumeration of all integers starting from 1 up to the argument.
(defun interval (tensor))
