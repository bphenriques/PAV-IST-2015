(defun promoting-call (f-original f-apply x y)
	(multiple-value-bind (xp yp)
		(promote x y)
		(funcall f-original f-apply xp yp)))

(defgeneric promote (x y)
	(:method ((x t) (y t))
		(error "No promotion for args (~S ~S) of classes (~S ~S)"
				x y
				(class-name (class-of x)) (class-name (class-of y)))))

(defmethod promote ((x tensor-vector) (y tensor-scalar))
	(values x
		(apply #'v (make-list (array-dimension (tensor-content x) 0) :initial-element (tensor-content y)))))

(defmethod promote ((x tensor-scalar) (y tensor-vector))
	(values (apply #'v (make-list (array-dimension (tensor-content y) 0) :initial-element (tensor-content x)))
			y))

