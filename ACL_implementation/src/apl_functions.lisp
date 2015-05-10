;;;; apl_functions.lisp
;;;;
;;;; Defines APL-like functions, specifically the ones which can be used
;;;; in either monadic or dyadic forms.
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
     the corresponding elements of the argument tensors."
     (labels ((apl-simetric (n)
                  (- n)))
      (let ((n-args (length tensors)))
          (cond ((= 1 n-args)
                 (map-tensor #'apl-simetric (car tensors)))
                ((= 2 n-args)
                 (map-tensor #'- (car tensors) (second tensors)))
                (t (error ".- : Wrong number of arguments"))))))


(defun ./ (&rest tensors)
    "Can either receive 1 or 2 tensors as arguments.

     If 1 tensor is given, it creates a tensor whose elements are the
     inverse of the corresponding elements of the argument tensor.

     If 2 tensors are given, it creates a tensor with the division of
     the corresponding elements of the argument tensors."
     (labels ((apl-inverse (n)
                (if (= n 0)
                    0
                    (/ 1 n))))
      (let ((n-args (length tensors)))
          (cond ((= 1 n-args)
                 (map-tensor #'apl-inverse (car tensors)))
                ((= 2 n-args)
                 (map-tensor #'/ (car tensors) (second tensors)))
                (t (error "./ : Wrong number of arguments"))))))
