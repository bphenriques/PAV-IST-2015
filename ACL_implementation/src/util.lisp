;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Internal funtions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-tensor (function &rest tensors)
    (let ((num-tensors (length tensors)))
        (cond ((= num-tensors 1)
               (map-single function (car tensors)))
              ((= num-tensors 2)
               (map-double function (car tensors) (car (cdr tensors))))
              (t (error "Apply only supports one or two tensors")))))


(defgeneric map-single (function t1)
    (:method ((function t) (t1 t))
        (error "Not supported")))

(defmethod map-single (function (t1 tensor-scalar))
    (s (funcall function (tensor-content t1))))

(defmethod map-single (function (t1 tensor-vector))
    (let ((original-vector (tensor-content t1)))
        (apply #'v (map 'list function original-vector))))


(defgeneric map-double (function t1 t2)
    (:method ((function t) (t1 t) (t2 t))
        (multiple-value-bind (t1p t2p)
            (promote t1 t2)
            (funcall #'map-double function t1p t2p))))

(defmethod map-double (function (t1 tensor-scalar) (t2 tensor-scalar))
    (s (funcall function (tensor-content t1) (tensor-content t2))))

(defmethod map-double (function (t1 tensor-vector) (t2 tensor-vector))
    (let*  ((c1 (map 'list (lambda (scal) (tensor-content scal)) (tensor-content t1)))
            (c2 (map 'list (lambda (scal) (tensor-content scal)) (tensor-content t2)))
            (len1 (length c1))
            (len2 (length c2)))

        (when (not (= len1 len2))
            (error "Cannot apply operators to vectors of different dimensions"))
		
        (apply #'v (map 'list function c1 c2))))

(defun print-n-lines (n stream)
    (dotimes (i n)
        (format stream "~%")))
        
(defun array-slice (arr row)
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
       :displaced-index-offset (* row (array-dimension arr 1))))
        
    
