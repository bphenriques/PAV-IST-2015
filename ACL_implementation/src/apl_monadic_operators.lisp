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
		(error "fold: Argument is not a function. Got ~S"
			(get-class-name func))))

(defmethod fold ((func function))
	(lambda (vec)
		(when (not (tensor-vector-p vec))
			(error "fold: Argument must be a vector but got ~S."
				(get-class-name vec)))
		(s (reduce func (array-to-list (tensor-content vec))))))


;;; scan method declarations
(defgeneric scan (func)
	(:documentation
		"Returns a function that, given a vector, computes the application
		 of the function given, to increasingly large subsets of the elements of the
		 vector, starting from a subset containing just the first element up to
		 a subset containing all elements.")
	(:method ((func t))
		(error "scan: Argument is not a function. Got ~S"
			(get-class-name func))))


(defmethod scan ((func function))
	(lambda (vec)
		(let* ((lst (array-to-list (tensor-content vec)))
			   (len (length lst))
			   (result (list)))
		(dotimes (l len)
					(setf result (cons (reduce func lst) result))
					(setf lst (butlast lst)))
			(apply #'v result))))


(defun outer-product (function)
	(lambda (tensor1 tensor2)
		(outer-product-aux function tensor1 tensor2)))


;;; outer-product method declarations
(defgeneric outer-product-aux (func tensor1 tensor2)
	(:documentation
		"Returns a function that, given two tensors, returns a new tensor with
		 the result of applying the function to every combination of values
		 from the first and second tensors.")
	(:method ((func t) (tensor1 t) (tensor2 t))
		(error "outer-product: Argument is not a function. Got ~S"
			(get-class-name func))))

(defmethod outer-product-aux ((func function) (tensor1 tensor-scalar) (tensor2 tensor))
	(funcall func tensor1 tensor2))

(defmethod outer-product-aux ((func function) (tensor1 tensor) (tensor2 tensor-scalar))
	(funcall func tensor1 tensor2))

(defmethod outer-product-aux ((func function) (tensor1 tensor) (tensor2 tensor))
	(let* ((number-elements (apply #'+ (tensor-dimensions tensor1)))
		   (args (expand-tensor tensor1))
		   (result (create-tensor (cons number-elements
							  			(tensor-dimensions tensor2))))
		   (result-content (tensor-content result)))
		(dotimes (i number-elements)
				 (setf (aref result-content i)
				  	   (funcall func (s (aref args i)) tensor2)))
		result))
