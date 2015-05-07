;;;; acl_structures.lisp
;;;;
;;;; Defines all additional strutures needed to implement APL like
;;;; behaviour, namely tensors.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


;;; Tensor Definition
(defstruct (tensor (:copier nil))
    "Represents an array of values.
     Content contains a vector of tensors,
     which values must be fetched recursively."
    content
    (dimensions nil :type list))

(defmethod print-object ((object tensor) stream)
   (let* ((dimensions (tensor-dimensions object))
          (dimensions-number (length dimensions)))
    (dotimes (i (first dimensions))
        (format stream "~S" (aref (tensor-content object) i))
        (when (not (eql i (- (first dimensions) 1)))
              (print-n-lines (- dimensions-number 1) stream)))))




;;; Scalar Definition
(defstruct (tensor-scalar
            (:include tensor)))

(defmethod print-object ((object tensor-scalar) stream)
    (format stream "~D" (tensor-scalar-content object)))


;;; Vector Definition
(defstruct (tensor-vector
            (:include tensor)))

(defmethod print-object ((object tensor-vector) stream)
   (let* ((dimensions (tensor-dimensions object)))
    (dotimes (i (first dimensions))
        (format stream
                (if (eql i (- (first dimensions) 1))
                    "~S"
                    "~S ")
                (aref (tensor-content object) i)))))

;;; Matrix defenition
(defstruct (tensor-matrix
        (:include tensor)))

(defmethod print-object ((object tensor-matrix) stream)
   (let* ((dimensions (tensor-dimensions object)))
    (dotimes (i (first dimensions))
        (format stream
                (if (eql i (- (first dimensions) 1))
                    "~S"
                    "~S~%")
                (aref (tensor-content object) i)))))



;;; Copy tensor functions
(defgeneric copy-tensor (tensor)
    (:method ((tensor tensor))
        (make-tensor :content (copy-array (tensor-content tensor))
                     :dimensions (copy-list (tensor-dimensions tensor)))))

(defmethod copy-tensor ((tensor tensor-scalar))
    (make-tensor-scalar :content (tensor-content tensor)
                        :dimensions (copy-list (tensor-dimensions tensor))))

(defmethod copy-tensor ((tensor tensor-vector))
    (make-tensor-vector :content (copy-array (tensor-content tensor))
                        :dimensions (copy-list (tensor-dimensions tensor))))

(defmethod copy-tensor ((tensor tensor-matrix))
    (make-tensor-matrix :content (copy-array (tensor-content tensor))
                        :dimensions (copy-list (tensor-dimensions tensor))))


;;; Map-tensor functions
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create a scalar
(defun s (value)
    (make-tensor-scalar :content value :dimensions '(1)))

(defun v (&rest values)
	(setf values (map 'list (lambda (x) (s x)) values))
    (make-tensor-vector :content (make-array (length values) :initial-contents values) :dimensions (list (length values))))



;(defun m (content)
;   (make-tensor-multi-dimension :content content))


(defun s-to-t (scalar dimensions)
    (cond ((eq (length dimensions) 1) (promoter scalar (first dimensions)))
          (t (promoter (s-to-t scalar (rest dimensions)) (first dimensions)))))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROMOTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric promoter (tensor dimension)
    (:method ((tensor tensor) dimension)
        (make-tensor
            :content (make-array (list dimension) :initial-element tensor)
            :dimensions (cons dimension (tensor-dimensions tensor)))))
        
(defmethod promoter ((tensor tensor-scalar) dimension)
    (apply #'v (make-list dimension :initial-element (tensor-content tensor))))
    
(defmethod promoter ((tensor tensor-vector) dimension)
    (make-tensor-matrix 
        :content (make-array (list dimension) :initial-element tensor)
        :dimensions (cons dimension (tensor-dimensions tensor))))
        
        
        
            
        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROMOTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric promote (x y)
    (:method ((x t) (y t))
        (error "No promotion for args (~S ~S) of classes (~S ~S)"
                x y
                (class-name (class-of x)) (class-name (class-of y)))))

(defmethod promote ((x tensor) (y tensor-scalar))
    (values x (s-to-t y (tensor-dimensions x))))
    
    
(defmethod promote ((x tensor-scalar) (y tensor))
    (values (s-to-t x (tensor-dimensions y)) y))


    
    
