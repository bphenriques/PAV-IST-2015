;;;; acl_monadic_functions.lisp
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
    "Same as the previous one, but using the factorial."
    (map-tensor #'fact tensor))

;Same as the previous one, but using the sin function
(defun .sin (tensor)
    (map-tensor #'sin tensor))

;Same as the previous one, but using the cos function.
(defun .cos (tensor)
    (map-tensor #'cos tensor))

;Same as the previous one, but using the negation. The result is a tensor containing, as element, the integer 0 or 1, depending on the corresponding element in the arugment tensor being different that zero or equal to zero.
(defun .not (tensor)
    (map-tensor (lambda (n)
                    (negate (create-bool n))) tensor))

;Creates a vector containing the length of each dimension of the argument tensor.
(defun shape (tensor)
    (apply #'v (tensor-dimensions tensor)))

;Creates a vector containing an enumeration of all integers starting from 1 up to the argument.
(defun interval (n)
    ;; FIXME - Verificar se o argumento é positivo
    (let ((tensor (apply #'v (make-list n :initial-element 0))))
        (print "starting")
        (map-tensor
            (let ((value 1))
                (lambda (n)
                    (let ((old value))
                        (incf value)
                        old)))
        tensor)))
