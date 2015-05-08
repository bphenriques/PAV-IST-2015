;;;; acl_functions.lisp
;;;;
;;;; Defines APL-like functions, which can be used in either monadic
;;;; or dyadic forms.
;;;; 
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


(defun .- (&rest tensors)
    "Can either receive 1 or 2 tensors as arguments.
    
     If 1 tensor is given, it creates a tensor whose elements are the 
     symmetric of the corresponding elements of the argument tensor.
     
     If 2 tensors are given, it creates a tensor with the subtraction of
     the corresponding elements of the argument tensors. If the 
     arguments are tensors with the same size and shape, the result 
     tensor will have that same size and shape. If one of the arguments
     is a scalar, the result tensor will have the same size and shape of
     the other argument and will have, as elements, the sum of the 
     scalar with every element of the other argument. Otherwise, the 
     function signals an error."
    (let ((n-args (length tensors)))
        (cond ((= 1 n-args)
               (map-tensor #'simetric (car tensors)))
              ((= 2 n-args)
               (map-tensor #'- (car tensors) (second tensors)))
              (t (error "Wrong number of arguments")))))


(defun ./ (&rest tensors)
    "Can either receive 1 or 2 tensors as arguments.
    
     If 1 tensor is given, it creates a tensor whose elements are the 
     inverse of the corresponding elements of the argument tensor.
     
     If 2 tensors are given, it creates a tensor with the division of
     the corresponding elements of the argument tensors. If the 
     arguments are tensors with the same size and shape, the result 
     tensor will have that same size and shape. If one of the arguments
     is a scalar, the result tensor will have the same size and shape of
     the other argument and will have, as elements, the sum of the 
     scalar with every element of the other argument. Otherwise, the 
     function signals an error."
    (let ((n-args (length tensors)))
        (cond ((= 1 n-args)
               (map-tensor #'inverse (car tensors)))
              ((= 2 n-args)
               (map-tensor #'/ (car tensors) (second tensors)))
              (t (error "Wrong number of arguments")))))
