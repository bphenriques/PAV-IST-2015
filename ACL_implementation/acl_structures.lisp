;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-tensor (tensor stream)
	(:method ((tensor t) (stream t))
		(error "ERRRRRORRRR")))

(defmethod print-sensor ((tensor tensor-scalar) (stream stream))
	(format stream "~D " (tensor-scalar-content tensor)))

(defmethod print-sensor ((tensor tensor-vector) (stream stream))
	(let* ((content (tensor-vector-content tensor))
										   (len (array-dimension content 0)))
									   (dotimes (l len)
											(format stream "~D " (aref content l)))))

(defstruct (tensor-scalar
				(:print-object (lambda (tensor stream)
									(print-sensor tensor stream))))
				content)

(defstruct (tensor-vector
				(:print-object (lambda (tensor stream)
									(print-sensor tensor stream))))
				content)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create a scalar
(defun s (value)
	(make-tensor-scalar :content value))

(defun v (&rest values)
	(make-tensor-vector :content values))
