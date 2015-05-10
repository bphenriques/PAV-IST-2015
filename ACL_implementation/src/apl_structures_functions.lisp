;;;; apl_structure_functions.lisp
;;;;
;;;; Contains functions related to tensor information and manipulation.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


;;; Tensor-dimensions methods
(defgeneric tensor-dimensions (tensor)
    (:documentation
        "Returns the length of each dimension of the given tensor.
         If the tensor given is a tensor-scalar, nil is returned."))

(defmethod tensor-dimensions ((tensor tensor-scalar)) nil)

(defmethod tensor-dimensions ((tensor tensor))
    (cons (length (tensor-content tensor))
          (tensor-dimensions (aref (tensor-content tensor) 0))))


;;; Expand-tensor methods
(defgeneric expand-tensor (tensor)
    (:documentation
        "Returns an array vector, filled with the elements of the given tensor."))

(defmethod expand-tensor ((tensor tensor-scalar))
    (make-array (list 1) :initial-contents (list (tensor-content tensor))))

(defmethod expand-tensor ((tensor tensor))
    (let* ((tensor-content (tensor-content tensor))
           (length-content (length tensor-content))
           (result (vector)))
           (dotimes (i length-content)
                (setf result (concatenate 'vector result (expand-tensor (aref tensor-content i)))))
           result))


;;; Promoter methods
(defgeneric promoter (tensor length)
    (:documentation
        "Returns a tensor one dimension higher than the provided tensor, with the
         length given. Each element of this new dimension is a copy of the given
         tensor.")
    (:method ((tensor tensor) length)
        (let ((elements (make-array (list length))))
            (dotimes (i length)
                (setf (aref elements i) (copy-tensor tensor)))
            (make-tensor :content elements))))

(defmethod promoter ((tensor tensor-scalar) length)
    (let ((elements (make-array (list length))))
        (dotimes (i length)
            (setf (aref elements i) (copy-tensor tensor)))
        (make-tensor-vector
            :content elements)))

(defmethod promoter ((tensor tensor-vector) length)
    (let ((elements (make-array (list length))))
        (dotimes (i length)
            (setf (aref elements i) (copy-tensor tensor)))
        (make-tensor-matrix
            :content elements)))


;;; map-tensor methods
(defun map-tensor (function &rest tensors)
    "Returns the resulting tensor of applying the function given to each element
     of the tensor(s) provided. If the arguments are tensors with the same size 
     and shape, the result tensor will have that same size and shape. If one of them
     is scalar, it applies the function using the scalar and the elment of the vector.
     Otherwise error."
    (let ((num-tensors (length tensors)))
        (cond ((= num-tensors 1)
               (map-single function (car tensors)))
              ((= num-tensors 2)
               (map-double function (car tensors) (second tensors)))
              (t (error "map-tensor: Apply only supports one or two tensors")))))


(defgeneric map-single (function t1)
    (:documentation
        "Returns the resulting tensor of applying the function given to each element
         of the tensor provided.")
    (:method ((function t) (t1 t))
        (error "map-single: Not supported for type ~S" (class-name (class-of t1)))))


(defmethod map-single (function (t1 tensor))
    (let* ((new-tensor (copy-tensor t1))
           (dimension (first (tensor-dimensions t1)))
           (new-tensor-content (tensor-content new-tensor)))
        (dotimes (i dimension)
            (let ((content (aref new-tensor-content i)))
                (setf (aref new-tensor-content i) (map-single function content))))
        new-tensor))


(defmethod map-single (function (t1 tensor-scalar))
    (s (funcall function (tensor-content t1))))


(defgeneric map-double (function t1 t2)
    (:documentation
        "Returns the resulting tensor of applying the function given to each
         corresponding element of both tensors provided.")
    (:method ((function t) (t1 t) (t2 t))
        (multiple-value-bind (t1p t2p)
            (promote t1 t2)
            (funcall #'map-double function t1p t2p))))

(defmethod map-double (function (t1 tensor-scalar) (t2 tensor-scalar))
    (s (funcall function (tensor-content t1) (tensor-content t2))))

(defmethod map-double (function (t1 tensor-scalar) (t2 tensor))
    (multiple-value-bind (t1p t2p)
        (promote t1 t2)
        (funcall #'map-double function t1p t2p)))

(defmethod map-double (function (t1 tensor) (t2 tensor-scalar))
    (multiple-value-bind (t1p t2p)
        (promote t1 t2)
        (funcall #'map-double function t1p t2p)))

(defmethod map-double (function (t1 tensor) (t2 tensor))
    (let* ((new-tensor (copy-tensor t1))
           (dimension-t1 (first (tensor-dimensions t1)))
           (dimension-t2 (first (tensor-dimensions t2)))
           (new-tensor-content (tensor-content new-tensor)))

        (cond ((not (equal dimension-t1 dimension-t2))
               (error "map-double: tensors must have same dimensions."))
              (t (dotimes (i dimension-t1)
                    (let ((content-t1 (aref (tensor-content t1) i))
                          (content-t2 (aref (tensor-content t2) i)))
                      (setf (aref new-tensor-content i)
                            (map-double function content-t1 content-t2))))
                new-tensor))))


;;; delete-last-dimension-nth-el methods
(defgeneric delete-last-dimension-nth-el (tensor n)
    (:method ((tensor t) (n t))
        (error "delete-last-dimension-nth-el: only supports a tensor and a integer but got ~S ~S" (class-name (class-of tensor)) (class-name (class-of n)))))

(defmethod delete-last-dimension-nth-el ((tensor tensor-scalar) (n integer))
    tensor)

(defmethod delete-last-dimension-nth-el ((tensor tensor-vector) (n integer))
    (let ((content (tensor-content tensor)))
        (setf (tensor-content tensor) (delete-if (let ((count n))
                        (lambda (x)
                            (declare (ignore x))
                            (decf count)
                            (< count 0)))
                content
                :count 1))))

(defmethod delete-last-dimension-nth-el ((tensor tensor) (n integer))
    (let ((content (tensor-content tensor)))
        (dotimes (i (length content))
            (delete-last-dimension-nth-el (aref content i) n))))

(defgeneric get-last-dimension-row-vector (tensor slice))

(defmethod get-last-dimension-row-vector ((tensor tensor-matrix) slice)
    (aref (tensor-content tensor) slice))

(defmethod get-last-dimension-row-vector ((tensor tensor) slice)
    (let* ((content (tensor-content tensor))
           (dimension-length (first (tensor-dimensions tensor)))
           (top-index-slice (integer-division slice dimension-length))
           (bot-index-slice (remainder-integer-division slice dimension-length)))
        (get-last-dimension-row-vector (aref content top-index-slice) bot-index-slice)))


;;; get-last-dimension-slide method
(defgeneric get-last-dimension-slice (tensor slice))

(defmethod get-last-dimension-slice ((tensor tensor-vector) slice)
    (tensor-content (aref (tensor-content tensor) slice)))

(defmethod get-last-dimension-slice ((tensor tensor) slice)
    (let* ((dimension-length (first (tensor-dimensions tensor)))
           (content (tensor-content tensor))
           (result (list)))
        (dotimes (i dimension-length)
            (let ((dimension-slice (get-last-dimension-slice (aref content i) slice)))
                (setf result (append result (if (listp dimension-slice)
                                                dimension-slice
                                                (list dimension-slice))))))
        result))
