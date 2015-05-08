(defgeneric create-bool (n)
	(:method ((n t))
		(error "create-bool: Only supports t, nil or a number")))

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

(defmethod negate ((n number))
	(if (= n 0)
		1
		0))