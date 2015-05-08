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
    content)

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
        (make-tensor :content (copy-array (tensor-content tensor)))))

(defmethod copy-tensor ((tensor tensor-scalar))
    (make-tensor-scalar :content (tensor-content tensor)))

(defmethod copy-tensor ((tensor tensor-vector))
    (make-tensor-vector :content (copy-array (tensor-content tensor))))

(defmethod copy-tensor ((tensor tensor-matrix))
    (make-tensor-matrix :content (copy-array (tensor-content tensor))))


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

(defgeneric s (value)
  (:method ((value t))
    (error "s: Only supports numbers or another scalar")))

(defmethod s ((value tensor-scalar))
  value)

(defmethod s ((value number))
  (make-tensor-scalar :content value))


(defun v (&rest values)
    (cond ((null values) nil)
          (t (setf values (map 'list (lambda (x) (s x)) values))
             (make-tensor-vector :content (make-array (length values) :initial-contents values)))))

(defgeneric tensor-set (tensor value &rest values))

(defmethod tensor-set ((tensor tensor-scalar) value &rest values)
    (if (not (null values))
        (error "Scalars don't accept coordinates for set")
        (progn (print "foofoo")(setf (tensor-content tensor) value))))

(defmethod tensor-set ((tensor tensor) value &rest values)
    (apply #'tensor-set (aref (tensor-content tensor) (first values)) value (rest values))
    tensor
)      

(defgeneric tensor-ref (tensor &rest values))

(defmethod tensor-ref ((tensor tensor-scalar) &rest values)
    (if (not (null values))
        (error "Too many coordinates.")
        (tensor-content tensor)))
    
(defmethod tensor-ref ((tensor tensor) &rest values)
    (apply #'tensor-ref (aref (tensor-content tensor) (first values)) (rest values)))

(defun create-tensor (dimensions &optional (initial-value 0))
   (s-to-t (s initial-value) dimensions))


(defun s-to-t (scalar dimensions)
    (cond ((eq (length dimensions) 1) (promoter scalar (first dimensions)))
          (t (promoter (s-to-t scalar (rest dimensions)) (first dimensions)))))

(defgeneric tensor-dimensions (tensor))

(defmethod tensor-dimensions ((tensor tensor-scalar)) nil)

(defmethod tensor-dimensions ((tensor tensor))
    (cons (array-dimension (tensor-content tensor) 0)
          (tensor-dimensions (aref (tensor-content tensor) 0))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROMOTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


    
    
