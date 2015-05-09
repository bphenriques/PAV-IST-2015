; ct Accepts two functions and returns a function that, given two
; tensors, returns a new tensor computed according to the rules of the algebraic inner product but replacing the algebraic sum and product with
; the first and second functions.

; TODO, DAR ERRO SE NAO TIVER DIMENSOES IGUAIS?

(defgeneric inner-product (f1 f2)
	(:method ((f1 t) (f2 t))
		(error "inner-product: both arguments must be functions")))

(defmethod inner-product ((f1 function) (f2 function))
	(lambda (t1 t2)
		(let ((tc1 (copy-tensor t1))
			  (tc2 (copy-tensor t2)))
		(inner-product-aux tc1 tc2 f1 f2))))

(defgeneric inner-product-aux (t1 t2 f1 g2)
	(:method ((f1 t) (f2 t) (t1 t) (t2 t))
		(error "inner-product: both arguments must be functions and tensors")))

; makes fucking sense!
(defmethod inner-product-aux ((t1 tensor-vector) (t2 tensor-vector) (f1 function) (f2 function))
	(funcall (fold f1) (map-tensor f2 t1 t2)))


;hmmmmmmmmmm
(defmethod inner-product-aux ((t1 tensor) (t2 tensor) (f1 function) (f2 function))
	(let ((c1 (tensor-content t1))
		  (c2 (tensor-content t2))
		  (result nil))

	(dotimes (i (length c1))
		(setf result (funcall (fold f1) (inner-product-aux (aref c1 i) (aref c2 i) f1 f2))))

	result))



