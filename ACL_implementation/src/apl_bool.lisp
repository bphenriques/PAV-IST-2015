;;;; bool.lisp
;;;;
;;;; Contains functions to handle boolean representation, namely the
;;;; correspondance between booleans and the integers 1 and 0.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.

(defgeneric create-bool (n)
	(:documentation
		"Transforms given n to T or false, when 1 or 0, respectively. And also
		 vice-versa.")
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
		(error "negate: Only supports number")))

(defmethod negate ((n number))
	(if (= n 0)
		1
		0))

(defgeneric or-bool (b1 b2)
	(:method ((b1 t) (b2 t))
		(error "or-bool: Only supports numbers")))

(defmethod or-bool ((b1 number) (b2 number))
	(if (or (= b1 1) (= b2 1))
		1
		0))

(defgeneric and-bool (b1 b2)
	(:method ((b1 t) (b2 t))
		(error "and-bool: Only supports numbers")))

(defmethod and-bool ((b1 number) (b2 number))
	(if (and (= b1 1) (= b2 1))
		1
		0))
