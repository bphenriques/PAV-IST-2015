(defgeneric expand-tensor (tensor))

(defmethod expand-tensor ((tensor tensor-scalar))
    (make-array (list 1) :initial-contents (list (tensor-content tensor))))
    
(defmethod expand-tensor ((tensor tensor))
    (let* ((tensor-content (tensor-content tensor))
           (length-content (length tensor-content))
           (result (vector))
           )
           (dotimes (i length-content)
                (setf result (concatenate 'vector result (expand-tensor (aref tensor-content i))))
           )
           result
    )
)
