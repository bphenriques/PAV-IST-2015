; Accepts a function and returns another function that, given a vecotr, computes the application of the function to sucessive elements of the vector.
(defun fold ())

; Similar to fold but using increasingly large subsets of the eleemnts of the vector, starting from a subset containg just the first element up to a subset containing all elements
(defun scan ())

; Accepts a function and returns another functions taht, given two tensors, returns a new tensor with the result of applying the function to every combination of values from the first and second tesnsors
(defun outer_product ())