;;;; apl_monadic_functions.lisp
;;;;
;;;; Defines APL like monadic functions.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.

(defun .! (tensor)
    "Returns a tensor whose elements are the factorial of the corresponding
     elements of the argument tensor."
     (labels ((fact (n)
                (if (< n 2)
                    1
                    (* n (fact (- n 1))))))
        (map-tensor #'fact tensor)))

(defun .sin (tensor)
    "Returns a tensor whose elements are the sin of the corresponding
     elements of the argument tensor."
    (map-tensor #'sin tensor))

(defun .cos (tensor)
    "Returns a tensor whose elements are the cos of the corresponding
     elements of the argument tensor."
    (map-tensor #'cos tensor))

(defun .not (tensor)
    "Returns a tensor whose elements are the cos of the corresponding
     elements of the argument tensor."
    (map-tensor (lambda (n)
                    (negate (create-bool n))) tensor))

(defun shape (tensor)
    "Returns a vector containing the length of each dimension of the argument tensor."
    (apply #'v (tensor-dimensions tensor)))

(defun interval (n)
    "Returns a vector containing an enumeration of all integers starting from 1
     up to the argument."
    (when (< n 0)
        (error "interval: argument must be positive."))
    (let ((tensor (apply #'v (make-list n :initial-element 0))))
        (map-tensor
            (let ((value 1))
                (lambda (n)
                    (declare (ignore n))
                    (let ((old value))
                        (incf value)
                        old)))
        tensor)))
