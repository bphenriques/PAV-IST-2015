;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Printing mechanims
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tensor-scalar (tensor stream)
	(format stream "~D " (tensor-scalar-content tensor)))

(defun print-tensor-vector (tens stream)
	(let* ((content (tensor-vector-content tens))
		   (len (array-dimension content 0)))
		   (dotimes (l len)
				(format stream "~D " (aref content l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tensor content)

(defstruct (tensor-scalar 
				(:include tensor))
				(:print-object (lambda (tensor stream) 
									(format t "Calling scalar")
									(print-tensor-scalar tensor stream))))

(defstruct (tensor-vector 
				(:include tensor))
				(:print-object (lambda (tensor stream)
									(format t "Calling vector")
									(print-tensor-vector tensor stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create a scalar
(defun s (value)
	(make-tensor-scalar :content value))

(defun v (&rest values)
	(make-tensor-vector :content values))
