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
		(apply #'v (make-list (array-dimension (tensor-vector-content x) 0) :initial-element (tensor-scalar-content y)))))

(defmethod promote ((x tensor-scalar) (y tensor-vector))
	(values (apply #'v (make-list (array-dimension (tensor-vector-content y) 0) :initial-element (tensor-scalar-content x)))
			y))

(defun fact (n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(defun simetric (n)
	(- 0 n))

(defun inverse (n)
	(if (= n 0)
		0
		(/ 1 n)))