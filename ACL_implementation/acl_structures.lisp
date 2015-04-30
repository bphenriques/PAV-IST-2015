;(defmethod print-tensor ((tens tensor-vector) stream depth)
;	(declare (ignore depth))
;	(let* ((content (tensor-vector-cntent tens))
;		   (len (array-dimension content 0)))
;		   (dotimes (l len)
;				(format stream "~D " (aref content l)))))
				

(defstruct (tensor 
				(:print-function 
					(lambda (struct stream depth)
						(declare (ignore struct stream depth))
						(error "ERROR: No printing mechanism for default tensor"))))
	content)

(defstruct (tensor-scalar 
				(:include tensor)
				(:print-function 
					(lambda (struct stream depth)
						(declare (ignore depth))
						(format stream "~D " (tensor-scalar-content struct))))))
						

;create a scalar
(defun s (value)
	(make-tensor-scalar :content value))

;create a vectors
;(defun v (&rest values)
;	(make-tensor-vector :content values))
