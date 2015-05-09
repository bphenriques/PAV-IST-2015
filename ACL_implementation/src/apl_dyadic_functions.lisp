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
(defgeneric drop(tensor tensor2)
	(:method ((tensor tensor) tensor2)
		(let ((tensorContent (tensor-content tensor))
			   (tensorCopy1 nil)
			   (tensorCopy2 nil))
			(if (= 1 (length tensorContent))
				(drop (s (aref tensorContent 0)) tensor2)
				(progn 	(setf tensorCopy2 (drop (aref tensorContent 0) tensor2))
						(setf tensorCopy1 (drop (s 1) tensor))
						(dotimes (i (length (tensor-content tensorCopy2)))
							(setf (aref (tensor-content tensorCopy2) i) (drop tensorCopy1 (aref (tensor-content tensorCopy2) i)))
						)
						tensorCopy2)
			)
		)
	)
)

(defmethod drop ((tensor tensor-scalar) tensor2)
	(let ((remove-count (tensor-content tensor))
		   (tensorCopy (copy-tensor tensor2)))
		(if (< remove-count 0)

				(setf (tensor-content tensorCopy)
					(delete-if
						(lambda (&optional element) (declare (ignore element)) t)
						(tensor-content tensorCopy) :count (abs remove-count) :from-end t))

				(setf (tensor-content tensorCopy)
					(delete-if
						(lambda (&optional element) (declare (ignore element)) t)
						(tensor-content tensorCopy) :count (abs remove-count))))
		tensorCopy))

;Returns a tensor with the dimensions refered in the first argument,
; whose elements are taken from the second argument, repeating them if
; necessary to fill the resulting tensor.
(defun reshape (dimensions values)
    (let((cycler (get-cycler (expand-tensor values)))
		(result (create-tensor (array-to-list (expand-tensor dimensions)))))
			(map-tensor cycler result)))


(defgeneric catenate (tensor1 tensor2)
    (:documentation
        "If the two arguments are scalars, returns a vector containing those
         arguments. If the two arguments are tensors, returns a tensor that joins
         the arguments along the their last dimension."))

(defmethod catenate ((tensor1 tensor-scalar) (tensor2 tensor-scalar))
    (v (tensor-content tensor1) (tensor-content tensor2)))

(defmethod catenate ((tensor1 tensor-vector) (tensor2 tensor-vector))
    (let ((result (copy-tensor tensor1))
          (tensor2-copy (copy-tensor tensor2)))
        (setf (tensor-content result)
              (concatenate 'vector (tensor-content result)
                                   (tensor-content tensor2-copy)))
        result))

(defmethod catenate ((tensor1 tensor) (tensor2 tensor))
    (let* ((result (copy-tensor tensor1))
           (result-dimension-length (first (tensor-dimensions result)))
           (result-content (tensor-content result))
           (tensor1-content (tensor-content tensor1))
           (tensor2-content (tensor-content tensor2)))
        (when (not (equal (butlast (tensor-dimensions tensor1))
                          (butlast (tensor-dimensions tensor2))))
              (error "Dimensions not compatible."))
        (dotimes (i result-dimension-length)
            (setf (aref result-content i)
                  (catenate (aref tensor1-content i)
                            (aref tensor2-content i))))
        result))

; Returns a tensor of booleans with the same shape and dimension of the
; first argument, containing 1 for each element in the corresponding location
; in the first argument that occurs somewhere in the second argument and
; 0 otherwise.

(defun member? (tensor members)
	(let ((result (copy-tensor tensor))
		  (member-finder (get-member-finder (expand-tensor members))))
	  (map-tensor member-finder result)))

;From a tensor of booleans and another tensor, returns a tensor containing only the elements of the last dimension of the second argument whose corresponding element in the first tensor is 1.
(defun select (tensor-locations tensor)
  (let* ((tensor-copy (copy-tensor tensor))
         (lst-indexes (map 'list (lambda (x) (tensor-content x)) (array-to-list (tensor-content tensor-locations))))
         (pos 0)
         (times-deleted 0))
      (dolist (i lst-indexes)
          (format t "i: ~D~%" i)
          (when (= i 0)
              (format t "deleting ~D~%" (- pos times-deleted))
                (delete-last-dimension-nth-el tensor-copy (- pos times-deleted))
                (incf times-deleted))

          (format t "Copy after deleting: ~S~%" tensor-copy)
          (incf pos))
      tensor-copy))
