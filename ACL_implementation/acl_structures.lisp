;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-tensor (tensor stream)
	(:method ((tensor tensor) (stream stream))
		(error "ERROR........")))

(defmethod print-tensor ((tensor tensor-scalar) (stream stream))
	(format stream "~D " (tensor-scalar-content tensor)))


(defstruct (tensor 
				(:print-object (lambda (tensor stream)
						(print-tensor tensor stream))))
				content)


(defstruct (tensor-scalar 
				(:include tensor)))

(defstruct (tensor-vector 
				(:include tensor))
				(:print-object (lambda (tensor stream)
									(let* ((content (tensor-vector-content tensor))
										   (len (array-dimension content 0)))
									   (dotimes (l len)
											(format stream "~D " (aref content l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create a scalar
(defun s (value)
	(make-tensor-scalar :content value))

(defun v (&rest values)
	(make-tensor-vector :content values))
