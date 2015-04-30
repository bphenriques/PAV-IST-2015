(defun promoting-call (f x y)
	(multiple-value-bind (xp yp)
		(promote x y)
		(funcall f xp yp)))

(defgeneric promote (x y)
	(:method ((x t) (y t))
		(error "No promotion for args (~S ~S) of classes (~S ~S)"
				x y
				(class-name (class-of x)) (class-name (class-of y)))))


;(defmethod promote ((x tensor-array) (y tensor-scalar))
;	(values x
;		(make-array (array-dimensions x) :initial-element y)))

;(defmethod promote ((x tensor-scalar) (y tensor-array))
;	(values x
;		(make-array (array-dimensions x) :initial-element y)))