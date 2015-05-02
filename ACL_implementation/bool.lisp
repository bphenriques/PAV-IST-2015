(defstruct bool value)

(defmethod print-object ((object bool) stream)
	(if (bool-value object)
		(format stream "~D" 1)
		(format stream "~D" 0)))

(defgeneric create-bool (n)
	(:method ((n t))
		(make-bool :value n)))

(defmethod create-bool ((n bool))
	(make-bool :value (bool-value n)))

(defgeneric negate (bool)
	(:method ((bool t))
		(not bool)))

(defmethod negate ((bool bool))
	(setf (bool-value bool)  (not (bool-value bool))))