
;Creates a tensor whose elements are the symmetric of the corresponding elements of the argument tensor.
(defgeneric .- (tensor)
	(:method ((tensor t))
		(promoting-call #'.- tensor)))

(defmethod .- ((tensor tensor-scalar))
	(format t ".- at scalar"))

(defmethod .- ((tensor tensor-vector))
	(format t ".- at vector"))

;Same as the previous one, but using the inverse.
(defgeneric ./ (tensor))

;Same as the previous one, but using the factorial.
(defgeneric .! (tensor))

;Same as the previous one, but using the sin function
(defgeneric .sin (tensor)) 

;Same as the previous one, but using the cos function.
(defgeneric .cos (tensor))

;Same as the previous one, but using the negation. The result is a tensor containing, as element, the integer 0 or 1, depending on the corresponding element in the arugment tensor being different that zero or equal to zero.
(defgeneric .not (tensor)) 
 
;Creates a vector containing the length of each dimension of the argument tensor.
(defgeneric shape (tensor))

;Creates a vector containing an enumeration of all integers starting from 1 up to the argument.
(defgeneric interval (tensor))
