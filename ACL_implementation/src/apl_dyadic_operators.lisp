; ct Accepts two functions and returns a function that, given two
; tensors, returns a new tensor computed according to the rules of the algebraic inner product but replacing the algebraic sum and product with
; the first and second functions.

; TODO, DAR ERRO SE NAO TIVER DIMENSOES IGUAIS?

(defgeneric inner-product (f1 f2)
	(:method ((f1 t) (f2 t))
		(error "inner-product: both arguments must be functions")))

(defmethod inner-product ((f1 function) (f2 function))
	(lambda (t1 t2)
		(inner-product-step1 f1 f2 t1 t2)))


(defgeneric inner-product-step1 (f1 f2 t1 t2)
	(:method ((f1 t) (f2 t) (t1 t) (t2 t))
		(error "inner-product-step1: both arguments must be functions and tensors")))

; makes fucking sense!
(defmethod inner-product-step1 ((f1 function) (f2 function) (t1 tensor-scalar) (t2 tensor))
	(funcall f2 t1 t2))

; makes fucking sense!
(defmethod inner-product-step1 ((f1 function) (f2 function) (t1 tensor) (t2 tensor-scalar))
	(funcall f2 t1 t2))

;hmmmmmmmmmm
(defmethod inner-product-step1 ((f1 function) (f2 function) (t1 tensor) (t2 tensor))
	(let* ((last-dimension-t1 (car (last (tensor-dimensions t1))))
		   (result (inner-product-step2 f2 t1 t2 0)))

		(dolist (i (range last-dimension-t1 :min 1))
			(setf result (funcall f1 result (inner-product-step2 f2 t1 t2 i))))

	result))

(defgeneric inner-product-step2 (function t1 t2 slice)
	(:method ((function t) (t1 t) (t2 t) (slice t))
		(error "inner-product-step2: both arguments must be functions and tensors")))


(defmethod inner-product-step2 ((function function) (t1 tensor-scalar) (t2 tensor-scalar) slice)
	(declare (ignore slice))
	(funcall function t1 t2))

(defmethod inner-product-step2 ((function function) (t1 tensor) (t2 tensor) slice)
	(let* ((last-dimension-slice (get-last-dimension-slice t1 slice))
		   (t2-last-dimension-length (car (last (tensor-dimensions t2))))
		   (tensor1-arg (create-tensor (list (length last-dimension-slice)
											 t2-last-dimension-length)))
		   (tensor2-arg (create-tensor (list (length last-dimension-slice)
											 t2-last-dimension-length)))
		   (index 0))

		(dolist (value last-dimension-slice)
			(let ((vector (create-tensor (list t2-last-dimension-length) value)))
				(setf (aref (tensor-content tensor1-arg) index) vector))
			(incf index))

		(dotimes (i (length last-dimension-slice))
			(let ((vector (copy-tensor (get-last-dimension-row-vector t2 slice))))
				(setf (aref (tensor-content tensor2-arg) i) vector)))

		(funcall function tensor1-arg tensor2-arg)))
