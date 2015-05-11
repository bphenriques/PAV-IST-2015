;;;;;;;;;;;;      
;         
;extensions
;
; More changes were done in map-tensors to support multiple tensors as arguments
;
;;;;;;;;;;;;

(defun .expt (tensor1 tensor2 &rest tensors)
	(apply #'map-tensor #'expt tensor1 tensor2 tensors))

(defgeneric .set (tensor value &rest coordinates)
    (:documentation
        "Replaces the tensor's element in the given coordinates by the value provided.
         The number of coordinates must be equal to the number of dimensions of the
         given tensor."))

(defmethod .set ((tensor tensor-scalar) value &rest coordinates)
    (when (not (null coordinates))
		  (error "tensor-set: Scalars don't accept coordinates for set"))
      (setf (tensor-content tensor) value)
      tensor)

(defmethod .set ((tensor tensor) value &rest coordinates)
    (apply #'.set (aref (tensor-content tensor) (first coordinates)) value (rest coordinates))
    tensor)
    
(defmethod .get ((tensor tensor-scalar) &rest values)
	(when (not (null values))
		  (error "Too many coordinates."))
	(tensor-content tensor))
		
(defmethod .get ((tensor tensor) &rest values)
    (apply #'.get (aref (tensor-content tensor) (first values)) (rest values)))
