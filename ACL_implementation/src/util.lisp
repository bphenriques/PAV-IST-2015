;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Internal funtions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-array (array)
  "Returns a copy of array."
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))


(defun print-n-lines (n stream)
    (dotimes (i n)
        (format stream "~%")))

(defun array-slice (arr row)
    (make-array (array-dimension arr 1)
      :displaced-to arr
       :displaced-index-offset (* row (array-dimension arr 1))))


;;;;; RETIRADO NA NET - AINDA TENHO QUE PERCEBER E VER SE FUNCIONA - https://stackoverflow.com/questions/14758218/two-element-combinations-of-the-elements-of-a-list-inside-lisp-without-duplicat

(defun pair-with (elem lst)
  (mapcar (lambda (a) (list elem a)) lst))

(defun unique-pairs (lst)
  (mapcon (lambda (rest) (pair-with (car rest) (cdr rest)))
          (remove-duplicates lst)))
