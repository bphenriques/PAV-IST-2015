;;;; acl_structures.lisp
;;;;
;;;; Defines all additional strutures needed to implement APL like behaviour,
;;;; namely tensors.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


;;; Structure definitions

(defstruct (tensor (:copier nil))
    "Represents an array of values. Content contains a vector of tensors, which
     values must be fetched recursively."
    content)

(defstruct (tensor-scalar (:include tensor))
    "Represents a specific case of tensor, in which it has 0 dimensions, so
     it represents just a single element.
     Internally, content just contains the value, not tensors.")

(defstruct (tensor-vector (:include tensor))
    "Represents a specific case of tensor, in which it has just 1 dimension.")


(defstruct (tensor-matrix (:include tensor))
    "Represents a specific case of tensor, in which it has just 2 dimensions.")



;;; Print-object redefinitions
(defmethod print-object ((object tensor-scalar) stream)
    (format stream "~D" (tensor-scalar-content object)))

(defmethod print-object ((object tensor-vector) stream)
    (let* ((dimensions (tensor-dimensions object)))
        (dotimes (i (first dimensions))
            (format stream
                    (if (eql i (- (first dimensions) 1))
                        "~S"
                        "~S ")
                    (aref (tensor-content object) i)))))

(defmethod print-object ((object tensor-matrix) stream)
    (let* ((dimensions (tensor-dimensions object)))
        (dotimes (i (first dimensions))
            (format stream
                    (if (eql i (- (first dimensions) 1))
                        "~S"
                        "~S~%")
                        (aref (tensor-content object) i)))))

(defmethod print-object ((object tensor) stream)
    "Redefinition of print-object to conform with project specification.
     dsa"
    (let* ((dimensions (tensor-dimensions object))
           (dimensions-number (length dimensions)))
    (dotimes (i (first dimensions))
        (format stream "~S" (aref (tensor-content object) i))
        (when (not (eql i (- (first dimensions) 1)))
              (print-n-lines (- dimensions-number 1) stream)))))


;;; Copy tensor methods
(defgeneric copy-tensor (tensor)
    (:method ((tensor tensor))
        (make-tensor :content (tensor-vector-copy tensor))))

(defmethod copy-tensor ((tensor tensor-scalar))
    (make-tensor-scalar :content (tensor-content tensor)))

(defmethod copy-tensor ((tensor tensor-vector))
    (make-tensor-vector :content (tensor-vector-copy tensor)))


(defmethod copy-tensor ((tensor tensor-matrix))
    (make-tensor-matrix :content (tensor-vector-copy tensor)))

(defun tensor-vector-copy (tensor)
	(let ((tensorContent (tensor-content tensor))
		  (tensorList nil))
		(dotimes (i (length tensorContent))
			(setf tensorList (nconc tensorList (list (copy-tensor (aref tensorContent i))))))
		(make-array (length tensorContent) :initial-contents tensorList)))



;;; Internal contructors
(defun create-tensor (dimensions &optional (initial-value 0))
   (s-to-t (s initial-value) dimensions))


(defun s-to-t (scalar dimensions)
    (cond ((eq (length dimensions) 1) (promoter scalar (first dimensions)))
          (t (promoter (s-to-t scalar (rest dimensions)) (first dimensions)))))



;;; Getters and Setters
(defgeneric tensor-ref (tensor &rest values))

(defmethod tensor-ref ((tensor tensor-scalar) &rest values)
    (if (not (null values))
        (error "Too many coordinates.")
        (tensor-content tensor)))

(defmethod tensor-ref ((tensor tensor) &rest values)
    (apply #'tensor-ref (aref (tensor-content tensor) (first values)) (rest values)))

(defgeneric tensor-set (tensor value &rest values))

(defmethod tensor-set ((tensor tensor-scalar) value &rest values)
    (if (not (null values))
        (error "Scalars don't accept coordinates for set")
          (setf (tensor-content tensor) value)))

(defmethod tensor-set ((tensor tensor) value &rest values)
    (apply #'tensor-set (aref (tensor-content tensor) (first values)) value (rest values))
    tensor)



;;; Main APL Constructors
(defgeneric s (value)
  (:method ((value t))
    (error "s: Only supports numbers but got ~S" (class-name (class-of value)))))

(defmethod s ((value tensor-scalar))
  value)

(defmethod s ((value number))
  (make-tensor-scalar :content value))


(defun v (&rest values)
    (cond ((null values) nil)
          (t (setf values (map 'list (lambda (x) (s x)) values))
             (make-tensor-vector :content (make-array (length values) :initial-contents values)))))



;;; Promotion methods
(defgeneric promote (x y)
    (:method ((x t) (y t))
        (error "No promotion for args (~S ~S) of classes (~S ~S)"
                x y
                (class-name (class-of x)) (class-name (class-of y)))))

(defmethod promote ((x tensor) (y tensor-scalar))
    (values x (s-to-t y (tensor-dimensions x))))


(defmethod promote ((x tensor-scalar) (y tensor))
    (values (s-to-t x (tensor-dimensions y)) y))
