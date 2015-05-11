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


(defun .- (tensor1 &rest tensors)
    "Can either receive 1 or 2 tensors as arguments.

     If 1 tensor is given, it creates a tensor whose elements are the
     symmetric of the corresponding elements of the argument tensor.

     If more tensors are given, it creates a tensor with the subtraction of
     the corresponding elements of the argument tensors."
     (labels ((apl-simetric (n)
                  (- n)))
		(cond ((= 0 (length tensors))
                 (map-tensor #'apl-simetric tensor1))
               (t (apply #'map-tensor #'- tensor1 tensors)))))


(defun ./ (tensor1 &rest tensors)
    "Can either receive 1 or 2 tensors as arguments.

     If 1 tensor is given, it creates a tensor whose elements are the
     inverse of the corresponding elements of the argument tensor.

     If more tensors are given, it creates a tensor with the division of
     the corresponding elements of the argument tensors."
     (labels ((apl-inverse (n)
                (if (= n 0)
                    0
                    (/ 1 n))))
          (cond ((= 0 (length tensors))
                 (map-tensor #'apl-inverse tensor1))
                (t (apply #'map-tensor #'/ tensor1 tensors)))))
