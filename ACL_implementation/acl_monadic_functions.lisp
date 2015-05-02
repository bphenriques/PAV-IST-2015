;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Internal funtions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Create a copy of the tensor given by the arguments applying the function
(defgeneric create-tensor (function tensor)
	(:method ((function t) (tensor t))
		(promoting-call #'create-tensor function tensor)))

(defmethod create-tensor (function (tensor tensor-scalar))
	(format t ".- at scalar")
	(funcall function (tensor-scalar-content tensor)))

(defmethod create-tensor (function (tensor tensor-vector))
	(let ((result (list))
		  (original-vector (tensor-vector-content tensor)))
		(dotimes (i (array-dimension original-vector 0))
			(setf result (append result (list (funcall function (aref original-vector i))))))
		result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Creates a tensor whose elements are the symmetric of the corresponding elements of the argument tensor.
(defun .- (tensor)
	(create-tensor #'simetric tensor))

;Same as the previous one, but using the inverse.
(defun ./ (tensor)
	(create-tensor #'inverse tensor))

;Same as the previous one, but using the factorial.
(defun .! (tensor)
	(create-tensor #'fact tensor))

;Same as the previous one, but using the sin function
(defun .sin (tensor)
	(create-tensor #'sin tensor)) 

;Same as the previous one, but using the cos function.
(defun .cos (tensor)
	(create-tensor #'cos tensor))

;Same as the previous one, but using the negation. The result is a tensor containing, as element, the integer 0 or 1, depending on the corresponding element in the arugment tensor being different that zero or equal to zero.
(defun .not (tensor)) 
 
;Creates a vector containing the length of each dimension of the argument tensor.
(defun shape (tensor))

;Creates a vector containing an enumeration of all integers starting from 1 up to the argument.
(defun interval (tensor))
