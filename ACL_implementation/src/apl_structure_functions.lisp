;;;; apl_structure_functions.lisp
;;;;
;;;; Contains functions to manipulate tensors.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


;;; Tensor-dimensions methods
(defgeneric tensor-dimensions (tensor))

(defmethod tensor-dimensions ((tensor tensor-scalar)) nil)

(defmethod tensor-dimensions ((tensor tensor))
    (cons (length (tensor-content tensor))
          (tensor-dimensions (aref (tensor-content tensor) 0))))


;;; Expand-tensor methods
(defgeneric expand-tensor (tensor))

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
(defgeneric promoter (tensor dimension)
    (:method ((tensor tensor) dimension)
        (let ((elements (make-array (list dimension))))
            (dotimes (i dimension)
                (setf (aref elements i) (copy-tensor tensor)))
            (make-tensor :content elements))))

(defmethod promoter ((tensor tensor-scalar) dimension)
    (let ((elements (make-array (list dimension))))
        (dotimes (i dimension)
            (setf (aref elements i) (copy-tensor tensor)))
        (make-tensor-vector
            :content elements)))

(defmethod promoter ((tensor tensor-vector) dimension)
    (let ((elements (make-array (list dimension))))
        (dotimes (i dimension)
            (setf (aref elements i) (copy-tensor tensor)))
        (make-tensor-matrix
            :content elements)))



;;; Map-tensor methods
(defun map-tensor (function &rest tensors)
    (let ((num-tensors (length tensors)))
        (cond ((= num-tensors 1)
               (map-single function (car tensors)))
              ((= num-tensors 2)
               (map-double function (car tensors) (second tensors)))
              (t (error "Apply only supports one or two tensors")))))


(defgeneric map-single (function t1)
    (:method ((function t) (t1 t))
        (error "Not supported")))


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
               (error "Tensor dimensions not equal."))
              (t (dotimes (i dimension-t1)
                    (let ((content-t1 (aref (tensor-content t1) i))
                          (content-t2 (aref (tensor-content t2) i)))
                      (setf (aref new-tensor-content i)
                            (map-double function content-t1 content-t2))))
                new-tensor))))
