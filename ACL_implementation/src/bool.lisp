(defgeneric create-bool (n)
	(:method ((n t))
		(error "create-bool: Only supports tensor-scalar")))

; NAO DEVERIA SER NECESSARIO!
(defmethod create-bool ((n number))
	(if (= n 0)
		0
		1))

(defmethod create-bool ((n (eql t)))
	1)

(defmethod create-bool ((n (eql nil)))
	0)

(defgeneric negate (bool)
	(:method ((bool t))
		(error "negate: Only supports tensor-scalar")))

(defmethod negate ((n tensor-scalar))
	(if (= (tensor-content n) 0)
		1
		0))