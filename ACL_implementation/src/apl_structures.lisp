;;;; apl_structures.lisp
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
(defmethod print-object :around ((object tensor) stream)
    (declare (ignore stream))
    (when (not (null (tensor-content object)))
          (call-next-method)))

(defmethod print-object ((object tensor-scalar) stream)
    "Redefinition of print-object to conform with project specification.
     Prints the tensor-scalar's single element"
    (format stream "~D" (tensor-scalar-content object)))

(defmethod print-object ((object tensor-vector) stream)
    "Redefinition of print-object to conform with project specification.
     Prints the tensor-vector's elements on the same line,
     separated by one white space."
    (let* ((dimensions (tensor-dimensions object)))
        (dotimes (i (first dimensions))
            (format stream
                    (if (eql i (- (first dimensions) 1))
                        "~S"
                        "~S ")
                        (aref (tensor-content object) i)))))

(defmethod print-object ((object tensor-matrix) stream)
    "Redefinition of print-object to conform with project specification.
     Prints the tensor-matrix's rows as if they were vectors,
     separated by line breaks."
    (let* ((dimensions (tensor-dimensions object)))
        (dotimes (i (first dimensions))
            (format stream
                    (if (eql i (- (first dimensions) 1))
                        "~S"
                        "~S~%")
                        (aref (tensor-content object) i)))))

(defmethod print-object ((object tensor) stream)
    "Redefinition of print-object to conform with project specification.

     For each sub-tensor of tensor's first dimension, prints the sub-tensor
     separated from the next sub-tensor by a number of empty lines that is equal
     to the number of dimensions minus one."
    (let* ((dimensions (tensor-dimensions object))
           (dimensions-number (length dimensions)))
    (dotimes (i (first dimensions))
        (format stream "~S" (aref (tensor-content object) i))
        (when (not (eql i (- (first dimensions) 1)))
              (print-n-lines (- dimensions-number 1) stream)))))



;;; Copy tensor methods
(defgeneric copy-tensor (tensor)
    (:documentation "Returns a copy of the tensor given.")
    (:method ((tensor tensor))
        (make-tensor :content (tensor-vector-copy tensor))))

(defmethod copy-tensor ((tensor tensor-scalar))
    (make-tensor-scalar :content (tensor-content tensor)))

(defmethod copy-tensor ((tensor tensor-vector))
    (make-tensor-vector :content (tensor-vector-copy tensor)))

(defmethod copy-tensor ((tensor tensor-matrix))
    (make-tensor-matrix :content (tensor-vector-copy tensor)))

(defun tensor-vector-copy (tensor)
    "Returns a copy of tensor's content.
     Used internally to make sure whole objects are copied, not just the pointers."
	(let ((tensorContent (tensor-content tensor))
		  (tensorList nil))
		(dotimes (i (length tensorContent))
			(setf tensorList (nconc tensorList (list (copy-tensor (aref tensorContent i))))))
		(make-array (length tensorContent) :initial-contents tensorList)))



;;; Internal contructors
(defun create-tensor (dimensions &optional (initial-value 0))
    "Creates a tensor of the given dimensions.
     Dimensions must be given as a list where the 1st dimension
     is the head of the list.
     Optionally the initial value of the tensor elements can be given,
     defaulting to 0 if not provided."
    (if (and (eql (length dimensions) 1) (eql (first dimensions) 1))
        (s initial-value)
        (s-to-t (s initial-value) dimensions)))


(defun s-to-t (scalar dimensions)
    "Transforms a scalar to a tensor of the given dimensions, assigning to each
     element a copy of the scalar.
     Dimensions must be given as a list where the 1st dimension
     is the head of the list."
    (cond ((eq (length dimensions) 1) (promoter scalar (first dimensions)))
          (t (promoter (s-to-t scalar (rest dimensions)) (first dimensions)))))



;;; Getters and Setters
(defgeneric tensor-ref (tensor &rest coordinates)
    (:documentation
        "Returns the tensor's element in the given coordinates.
         The number of coordinates must be equal to the number of dimensions of the
         given tensor."))

(defmethod tensor-ref ((tensor tensor-scalar) &rest coordinates)
    (if (not (null coordinates))
        (error "tensor-ref: Too many coordinates.")
        (tensor-content tensor)))

(defmethod tensor-ref ((tensor tensor) &rest coordinates)
    (apply #'tensor-ref (aref (tensor-content tensor) (first coordinates)) (rest coordinates)))


(defgeneric tensor-set (tensor value &rest coordinates)
    (:documentation
        "Replaces the tensor's element in the given coordinates by the value provided.
         The number of coordinates must be equal to the number of dimensions of the
         given tensor."))

(defmethod tensor-set ((tensor tensor-scalar) value &rest coordinates)
    (if (not (null coordinates))
        (error "tensor-set: Scalars don't accept coordinates for set")
          (setf (tensor-content tensor) value)))

(defmethod tensor-set ((tensor tensor) value &rest coordinates)
    (apply #'tensor-set (aref (tensor-content tensor) (first coordinates)) value (rest coordinates))
    tensor)



;;; Promotion methods
(defgeneric promote (x y)
    (:documentation
        "Promotes one of the arguments to one functionally compatible
         with the other argument.")
    (:method ((x t) (y t))
        (error "promote: No promotion for args (~S ~S) of classes (~S ~S)"
                x y
                (get-class-name x) (get-class-name y))))

(defmethod promote ((x tensor) (y tensor-scalar))
    (values x (s-to-t y (tensor-dimensions x))))


(defmethod promote ((x tensor-scalar) (y tensor))
    (values (s-to-t x (tensor-dimensions y)) y))



;;; Main APL Constructors
(defgeneric s (value)
    (:documentation
        "Returns a newly created scalar with the value given.")
    (:method ((value t))
        (error "s: Only supports numbers but got ~S"
            (get-class-name value))))

(defmethod s ((value tensor-scalar))
  value)

(defmethod s ((value number))
  (make-tensor-scalar :content value))

(defun v (&rest values)
    "Returns a newly created vector with the values given.
     If just 1 value is given the resulting tensor IS STILL a vector,
     NOT a scalar."
    (cond ((null values) (make-tensor-vector))
          (t (setf values (map 'list (lambda (x) (s x)) values))
             (make-tensor-vector :content (make-array (length values) :initial-contents values)))))
