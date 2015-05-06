; Accepts a function and returns another function that, given a vecotr, computes the application of the function to sucessive elements of the vector.
(defgeneric fold (func)
	(:method ((func t))
		(error "Argument is not a function")))

(defmethod fold ((func function))
	(lambda (vec) (reduce func (map 'list (lambda (x) x) (tensor-content vec)))))
	


(defgeneric scan (func)
	(:method ((func t))
		(error "Argument is not a function")))

; Similar to fold but using increasingly large subsets of the eleemnts of the vector, starting from a subset containg just the first element up to a subset containing all elements
;(defun scan ((func function))
;	(lambda (vec) 
;		(let* ((lst (map 'list (lambda (x) x) (tensor-content vec)))
;			   (len (length lst)))
;			   
;			   nthcdr n list!!
;			(dotimes (n len)
				(
		
)

; Accepts a function and returns another functions taht, given two tensors, returns a new tensor with the result of applying the function to every combination of values from the first and second tesnsors
(defun outer_product ())
