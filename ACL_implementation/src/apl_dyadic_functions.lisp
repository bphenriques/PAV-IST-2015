;;;; apl_dyadic_functions.lisp
;;;;
;;;; Defines APL like dyadic functions.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.


(defun .+ (tensor1 tensor2 &rest tensors) 
    "Returns a tensor with the sum of the corresponding elements of the argument
     tensors."
    (apply #'map-tensor #'+ tensor1 tensor2 tensors))

(defun .* (tensor1 tensor2 &rest tensors)
    "Returns a tensor with the multiplication of the corresponding elements of
     the argument tensors."
     (apply #'map-tensor #'* tensor1 tensor2 tensors))

(defun .// (tensor1 tensor2 &rest tensors)
    "Returns a tensor with the integer division of the corresponding elements of
     the argument tensors."
     (apply #'map-tensor (lambda (n1 n2) (integer-division n1 n2)) tensor1 tensor2 tensors))

(defun .% (tensor1 tensor2 &rest tensors)
    "Returns a tensor with the integer division of the corresponding elements of
     the argument tensors."
     (apply #'map-tensor (lambda (n1 n2) (remainder-integer-division n1 n2)) tensor1 tensor2 tensors))

(defun .< (tensor1 tensor2)
    "Returns a tensor of booleans (represented by the integers 0 and 1), using
     the relation of 'less than' between the corresponding elements of
     the argument tensors."
    (map-tensor (lambda (n1 n2) (create-bool (< n1 n2))) tensor1 tensor2))

(defun .> (tensor1 tensor2)
    "Returns a tensor of booleans (represented by the integers 0 and 1), using
     the relation of 'greater than' between the corresponding elements of
     the argument tensors."
    (map-tensor (lambda (n1 n2) (create-bool (> n1 n2))) tensor1 tensor2))

(defun .<= (tensor1 tensor2)
    "Returns a tensor of booleans (represented by the integers 0 and 1), using
     the relation of 'less than or equal to' between the corresponding elements
     of the argument tensors."
    (map-tensor (lambda (n1 n2) (create-bool (<= n1 n2))) tensor1 tensor2))

(defun .>= (tensor1 tensor2)
    "Returns a tensor of booleans (represented by the integers 0 and 1), using
     the relation of 'more than or equal to' between the corresponding elements
     of the argument tensors."
    (map-tensor (lambda (n1 n2) (create-bool (>= n1 n2))) tensor1 tensor2))

(defun .= (tensor1 tensor2)
    "Returns a tensor of booleans (represented by the integers 0 and 1), using
     the relation of 'equal to' between the corresponding elements of
     the argument tensors."
    (map-tensor (lambda (n1 n2) (create-bool (= n1 n2))) tensor1 tensor2))

(defun .or (tensor1 tensor2)
    "Returns a tensor of booleans (represented by the integers 0 and 1), using
     the relation of logical disjunction between the corresponding elements of
     the argument tensors."
    (map-tensor (lambda (n1 n2)
                        (create-bool (or-bool n1 n2))) tensor1 tensor2))

(defun .and (tensor1 tensor2)
    "Returns a tensor of booleans (represented by the integers 0 and 1), using
     the relation of logical conjunction between the corresponding elements of
     the argument tensors."
    (map-tensor (lambda (n1 n2)
                        (create-bool (and-bool n1 n2))) tensor1 tensor2))

;;; Non-scalable Functions

(defgeneric drop(tensor tensor2)
    (:documentation
        "Accepts a scalar n1 or vector (of elements ni) and a non-scalar tensor and
        returns a tensor where the first (if n > 0) or last (if n < 0) n elements of
        the i dimension of the tensor were removed.")
	(:method ((tensor tensor) tensor2)
		(let ((content (tensor-content tensor))
			   (tensor-copy1 nil)
			   (tensor-copy2 nil))
			(cond ((= 0 (length content)) tensor2)
                  ((= 1 (length content))
                   (drop (s (aref content 0)) tensor2))
                  (t 	(setf tensor-copy2 (drop (aref content 0) tensor2))
						(setf tensor-copy1 (drop (s 1) tensor))
						(dotimes (i (length (tensor-content tensor-copy2)))
							(setf (aref (tensor-content tensor-copy2) i) (drop tensor-copy1 (aref (tensor-content tensor-copy2) i))))
						tensor-copy2)))))

(defmethod drop ((tensor tensor-scalar) tensor2)
	(let ((remove-count (tensor-content tensor))
		  (tensor-copy (copy-tensor tensor2)))
		(when (< (length (tensor-content tensor2)) remove-count)
				(error "Can't remove more elements than the tensor has"))
		(setf (tensor-content tensor-copy)
			  (delete-if
					(lambda (&optional element) (declare (ignore element)) t)
					(tensor-content tensor-copy) 
					:count (abs remove-count) 
					:from-end (< remove-count 0)))
		tensor-copy))


(defun reshape (dimensions values)
    "Returns a tensor with the dimensions refered in the first argument,
     whose elements are taken from the second argument, repeating them if
     necessary to fill the resulting tensor."
    (let ((cycler (get-cycler (expand-tensor values)))
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
              (error "catenate: Dimensions not compatible."))
        (dotimes (i result-dimension-length)
            (setf (aref result-content i)
                  (catenate (aref tensor1-content i)
                            (aref tensor2-content i))))
        result))



(defun member? (tensor members)
    "Returns a tensor of booleans with the same shape and dimension of the
     first argument, containing 1 for each element in the corresponding location
     in the first argument that occurs somewhere in the second argument and
     0 otherwise."
     (labels ((get-member-finder (member-vector)
                  (let ((members member-vector))
                    (lambda (n)
                      (create-bool (find n members))))))
    	(let ((result (copy-tensor tensor))
    		  (member-finder (get-member-finder (expand-tensor members))))
    	  (map-tensor member-finder result))))


(defgeneric select (tensor-locations tensor)
    (:documentation 
        "From a tensor of booleans and another tensor, returns a tensor containing
         only the elements of the last dimension of the second argument whose
         corresponding element in the first tensor is 1."))

(defmethod select ((tensor-locations tensor) (tensor tensor-scalar))
    (error "select function not applicable to scalar tensors."))
    
(defmethod select ((tensor-locations tensor-scalar) (tensor tensor))
    (let* ((tensor-copy (copy-tensor tensor))
           (value (tensor-content tensor-locations)))
           (when (= value 0)           
                 (delete-last-dimension-nth-el tensor-copy 0))
      tensor-copy))


(defmethod select ((tensor-locations tensor) (tensor tensor))
    (let* ((tensor-copy (copy-tensor tensor))
           (lst-indexes (map 'list (lambda (x) (tensor-content x)) (array-to-list (tensor-content tensor-locations))))
           (pos 0)
           (times-deleted 0))
       (dolist (i lst-indexes)
          (when (= i 0)
                (delete-last-dimension-nth-el tensor-copy (- pos times-deleted))
                (incf times-deleted))
          (incf pos))
      tensor-copy))
