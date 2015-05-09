;;;; acl_dyadic_functions.lisp
;;;;
;;;; Defines APL like dyadic functions.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


; Creates a tensor with the sum of the corresponding elements of the argument
; tensors. If the arguments are tensors with the same size and shape, the
; result tensor will have that same size and shape. If one of the arguments
; is a scalar, the result tensor will have the same size and shape of the other
; argument and will have, as elements, the sum of the scalar with every
; element of the other argument. Otherwise, the function signals an error.
(defun .+ (tensor1 tensor2)
    (map-tensor #'+ tensor1 tensor2))

; Same as the previous one, but using multiplication.
(defun .* (tensor1 tensor2)
    (map-tensor #'* tensor1 tensor2))

; Same as the previous one, but using integer division.
(defun .// (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (integer-division n1 n2)) tensor1 tensor2))

; Same as the previous one, but using the remainder of the integer division.
(defun .% (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (remainder-integer-division n1 n2)) tensor1 tensor2))

; Same as the previous one, but using the relation "less than." The result tensor will have, as elements, the integers 0 or 1.
(defun .< (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (create-bool (< n1 n2))) tensor1 tensor2))

; Same as the previous one, but using the relation "greater than."
(defun .> (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (create-bool (> n1 n2))) tensor1 tensor2))

; Same as the previous one, but using the relation "less than or equal to."
(defun .<= (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (create-bool (<= n1 n2))) tensor1 tensor2))

; Same as the previous one, but using the relation "greater than or equal to."
(defun .>= (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (create-bool (>= n1 n2))) tensor1 tensor2))

; Same as the previous one, but using the relation "equal to."
(defun .= (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (create-bool (= n1 n2))) tensor1 tensor2))

; Same as the previous one, but using the logical disjunction.
(defun .or (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (create-bool (or n1 n2))) tensor1 tensor2))

; Same as the previous one, but using the logical conjunction.
(defun .and (tensor1 tensor2)
    (map-tensor (lambda (n1 n2) (create-bool (and n1 n2))) tensor1 tensor2))

; Accepts a scalar n1 or vector (of elements ni) and a non-scalar tensor and
; returns a tensor where the first (if n > 0) or last (if n < 0) n elements of
; the i dimension of the tensor were removed.
(defun drop())

;Returns a tensor with the dimensions refered in the first argument,
; whose elements are taken from the second argument, repeating them if
; necessary to fill the resulting tensor.
(defun reshape (dimensions values)
    (let((cycler (get-cycler (expand-tensor values)))
		(result (create-tensor (array-to-list (expand-tensor dimensions)))))
			(map-tensor cycler result)))

; If the two arguments are scalars, returns a vector containing those
; arguments. If the two arguments are tensors, returns a tensor that joins
; the arguments along the their last dimension

(defun catenate ())

; Returns a tensor of booleans with the same shape and dimension of the
; first argument, containing 1 for each element in the corresponding location
; in the first argument that occurs somewhere in the second argument and
; 0 otherwise.

(defun member? (tensor members)
	(let ((result (copy-tensor tensor))
		  (member-finder (get-member-finder (expand-tensor members))))
	  (map-tensor member-finder result)))

;From a tensor of booleans and another tensor, returns a tensor containing only the elements of the last dimension of the second argument whose corresponding element in the first tensor is 1.
(defun select (tensor-vector-boolean tensor2)
	;(let())
)
