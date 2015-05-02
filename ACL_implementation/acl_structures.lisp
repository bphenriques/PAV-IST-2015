;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tensor-scalar content)

(defmethod print-object ((object tensor-scalar) stream)
	(format stream "~D " (tensor-scalar-content object)))



(defstruct tensor-vector content)

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