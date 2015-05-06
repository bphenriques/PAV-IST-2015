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


;;; Structures



(defstruct tensor
    (content nil :type array)
    (dimensions nil :type list))


(defmethod print-object ((object tensor) stream)
   (let* ((dimensions (tensor-dimensions object))
          (dimensions-number (length dimensions)))
          
    (dotimes (i (first dimensions))
        (format stream "~S " (aref (tensor-content object) i))
        (when (not (eql (- (first dimensions) 1)))
              (print-n-lines (- dimensions-number 1))))))
        


(defstruct (tensor-scalar
            (:include tensor)))

(defmethod print-object ((object tensor-scalar) stream)
    (format stream "~D " (tensor-scalar-content object)))

(defstruct (tensor-vector
            (:include tensor)))

(defmethod print-object ((object tensor-vector) stream)
    (let* ((content (tensor-vector-content object))
           (len (array-dimension content 0)))
       (dotimes (l len)
            (format stream "~D " (aref content l)))))



(defstruct (tensor-matrix
        (:include tensor)))

(defmethod print-object ((object tensor-matrix) stream)
    (let* ((content (tensor-content object))
           (len (array-dimension content 0)))
       (dotimes (i len)
            (format stream "~S " (aref content i)))))
       



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create a scalar
(defun s (value)
    (make-tensor-scalar :content value :dimensions '(1)))

(defun v (&rest values)
    (make-tensor-vector :content (make-array (length values) :initial-contents values) :dimensions (list (length values))))



;(defun m (content)
;   (make-tensor-multi-dimension :content content))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROMOTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric promote (x y)
    (:method ((x t) (y t))
        (error "No promotion for args (~S ~S) of classes (~S ~S)"
                x y
                (class-name (class-of x)) (class-name (class-of y)))))

(defmethod promote ((x tensor-vector) (y tensor-scalar))
    (values x
        (apply #'v (make-list (array-dimension (tensor-content x) 0) :initial-element (tensor-content y)))))

(defmethod promote ((x tensor-scalar) (y tensor-vector))
    (values (apply #'v (make-list (array-dimension (tensor-content y) 0) :initial-element (tensor-content x)))
            y))
