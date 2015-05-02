;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Internal funtions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


; Create a copy of the tensor given by the arguments applying the function
(defgeneric create-tensor-1 (function t1)
	(:method ((function t) (t1 t))
		(error "Not supported")))

(defmethod create-tensor-1 (function (t1 tensor-scalar))
	(s (funcall function (tensor-content t1))))

(defmethod create-tensor-1 (function (t1 tensor-vector))
	(let ((result (list))
		  (original-vector (tensor-content t1)))

		(dotimes (i (array-dimension original-vector 0))
			(setf result (append result (list (funcall function (aref original-vector i))))))
		; convert list to arguments
		(apply #'v result)))


; Create a copy of the tensor given by the arguments applying the function
(defgeneric create-tensor-2 (function t1 t2)
	(:method ((function t) (t1 t) (t2 t))
		(promoting-call #'create-tensor-2 function t1 t2)))

(defmethod create-tensor-2 (function (t1 tensor-scalar) (t2 tensor-scalar))
	(s (funcall function (tensor-content t1) (tensor-content t2))))

(defmethod create-tensor-2 (function (t1 tensor-vector) (t2 tensor-vector))
	(let*  ((c1 (tensor-content t1))
		   	(c2 (tensor-content t2))
			(len1 (array-dimension c1 0))
		  	(len2 (array-dimension c2 0))
		  	(result (list)))

		(when (not (= len1 len2))
			(error "Cannot apply operators to vectors of different dimensions"))

		(dotimes (i len1)
			(setf result (append result (list (funcall function (aref c1 i) (aref c2 i))))))

		(apply #'v result)))