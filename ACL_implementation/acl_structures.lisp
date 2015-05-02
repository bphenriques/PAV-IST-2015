;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct tensor content)

(defstruct (tensor-scalar
			(:include tensor)))

(defmethod print-object ((object tensor-scalar) stream)
	(format stream "~D " (tensor-scalar-content object)))

(defstruct (tensor-vector
			(:include tensor)))

(defmethod print-object ((object tensor-vector) stream)
	(let* ((content (tensor-vector-content object))
		   (len (array-dimension content 0)))
	   (dotimes (l len)
			(format stream "~D " (aref content l)))))

; Not used yet
(defstruct tensor-dimension content)

(defmethod print-object ((object tensor-dimension) stream)
	(let* ((content (tensor-matrix-content object)))
		(dolist (vector content)
			(format stream "~S ~%~%" vector))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create a scalar
(defun s (value)
	(make-tensor-scalar :content value))

(defun v (&rest values)
	(make-tensor-vector :content (make-array (length values) :initial-contents values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROMOTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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