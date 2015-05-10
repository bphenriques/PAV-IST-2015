;;;; apl_monadic_operators.lisp
;;;;
;;;; Defines APL like monadic operators.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.

;;; fold method declarations
(defgeneric fold (func)
	(:documentation
		"Returns a function that, given a vector, computes the application
		 of the function given, to sucessive elements of the vector.")
	(:method ((func t))
		(error "fold: Argument is not a function.")))

(defmethod fold ((func function))
	(lambda (vec)
		(when (not (tensor-vector-p vec))
			(error "fold: Argument must be a vector but got ~S." (class-name (class-of vec))))

		(s (reduce func (array-to-list (tensor-content vec))))))


;;; scan method declarations
(defgeneric scan (func)
	(:documentation
		"Returns a function that, given a vector, computes the application
		 of the function given, to increasingly large subsets of the elements of the
		 vector, starting from a subset containing just the first element up to
		 a subset containing all elements.")
	(:method ((func t))
		(error "scan: Argument is not a function.")))


(defmethod scan ((func function))
	(lambda (vec)
		(let* ((lst (map 'list (lambda (x) x) (tensor-content vec)))
			   (len (length lst))
			   (result (list)))
		(dotimes (l len)
					(setf result (cons (reduce func lst) result))
					(setf lst (butlast lst)))
				(apply #'v result))))



;;; outer-product method declarations
(defgeneric outer-product (func)
	(:documentation
		"Returns a function that, given two tensors, returns a new tensor with
		 the result of applying the function to every combination of values
		 from the first and second tensors.")
	(:method ((func t))
		(error "outer-product: Argument is not a function.")))

(defmethod outer-product ((func function))
	(lambda (tensor1 tensor2)
		(let* ((number-elements (apply #'+ (if (null (tensor-dimensions tensor1))
											   '(1)
											   (tensor-dimensions tensor1))))
			   (args (expand-tensor tensor1))
			   (result (create-tensor (cons number-elements
								  			(tensor-dimensions tensor2))))
			   (result-content (tensor-content result)))

			(cond ((eql number-elements 1)
				   (setf (tensor-content result)
					  	 (tensor-content (funcall func (s (aref args 0))
													   tensor2))))
				  (t (dotimes (i number-elements)
					 (setf (aref result-content i)
					  	   (funcall func (s (aref args i)) tensor2)))))
			result)))
