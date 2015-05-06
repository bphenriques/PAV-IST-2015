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



(defstruct tensor content)


;(defmethod print-object ((object tensor) stream)
 ;   (let* ((dimensions (tensor-dimensions tensor))
  ;         (dimensions-number (length dimensions)))
   ;     (dotimes (i (first dimensions))
     ;       (format stream "~S " ((tensor-scalar-content object)))
      ;      (print-n-lines (- dimensions-number 1)))))
        


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

;(defmethod print-object ((object tensor-matrix) stream)
    ;(let* ((content (tensor-content object))
           ;(dimensions (array-dimensions content)))
        ;(print-aux stream content dimensions)))

;(defun print-aux (stream content dimensions)
    ;(let ((num-dimensions (length dimensions)))
        ;(if (= (num-dimensions 2))
            ;;matrix
            ;(progn
                ;(dotimes (i num-dimensions)
                    ;(dotimes (j num-dimensions)
                        ;(format stream "~D" (aref content i j))
            ;(progn
                ;(dotimes (- num-dimensions 1)
                    ;(format stream "~%"))
                ;(print-aux stream content (cdr dimensions)))))))))




(defgeneric tensor-dimensions (tensor)
    (:method ((tensor t))
        (error "Argument is not a tensor.")))


(defmethod tensor-dimensions ((tensor tensor))
    (error "Not supported for that tensor."))

(defmethod tensor-dimensions ((tensor tensor-scalar))
    (list 1))

(defmethod tensor-dimensions ((tensor tensor-vector))
    (list (length (tensor-content tensor))))

;~ (defmethod tensor-dimensions ((tensor tensor-multi-dimension))
    ;~ (array-dimensions (tensor-content tensor)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;create a scalar
(defun s (value)
    (make-tensor-scalar :content value))

(defun v (&rest values)
    (make-tensor-vector :content (make-array (length values) :initial-contents values)))



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
