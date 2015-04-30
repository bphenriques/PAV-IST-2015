;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (tensor-scalar
				(:print-object (lambda (tensor stream)
									(format stream "~D " (tensor-scalar-content tensor)))))
				content)

(defstruct (tensor-vector
				(:print-object (lambda (tensor stream)
									(let* ((content (tensor-vector-content tensor))
										   (len (array-dimension content 0)))
									   (dotimes (l len)
											(format stream "~D " (aref content l)))))))
				content)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create a scalar
(defun s (value)
	(make-tensor-scalar :content value))

(defun v (&rest values)
	(make-tensor-vector :content (make-array (length values) :initial-contents values)))