; Accepts a function and returns another function that, given a vecotr, computes the application of the function to sucessive elements of the vector.
(defgeneric fold (func)
	(:method ((func t))
		(error "fold: Argument is not a function")))

(defmethod fold ((func function))
	(lambda (vec)
		(when (not (tensor-vector-p vec))
			(error "fold: Argument must be a vector but got ~S" (class-name (class-of vec))))
		
		(s (reduce func (array-to-list (tensor-content vec))))))

(defgeneric scan (func)
	(:method ((func t))
		(error "scan: Argument is not a function")))

; Similar to fold but using increasingly large subsets of the eleemnts of the vector, starting from a subset containg just the first element up to a subset containing all elements
(defmethod scan ((func function))
	(lambda (vec)
		(let* ((lst (map 'list (lambda (x) x) (tensor-content vec)))
			   (len (length lst))
			   (result (list)))
			   
		(dotimes (l len)
					(setf result (cons (reduce func lst) result))
					(setf lst (butlast lst)))
				(apply #'v result))))

; Accepts a function and returns another functions taht, given two tensors, returns a new tensor with the result of applying the function to every combination of values from the first and second tesnsors
(defgeneric outer_product (func)
	(:method ((func t))
		(error "Argument is not a function")))

(defmethod outer_product ((func function)) 
	(lambda (t1 t2)
		(let* ((dim1 (tensor-dimensions t1))
			   (dim2 (tensor-dimensions t2))
			   (cont1 (make-array (length dim1 :initial-content 0)))
			   (cont2 (make-array (length dim2 :initial-content 0)))))))

			
;(defgeneric combination_tensor (t1 t2)
	;(:method ((t1 t) (t2 t))
		;(error "Arguments are not a tensor!")))
		
;(defgeneric combination_tensor (t1 t2)
	;(:method ((t1 (tensor-scalar) (t2 t))
		;(error "Arguments are not a tensor!")))