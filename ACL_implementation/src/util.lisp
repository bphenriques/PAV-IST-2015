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

(defun equal-array-pos (array1 array2 x y max-x max-y)
    (let ((slot1 (aref array1 y x))
          (slot2 (aref array2 y x)))
      (cond ((not (equal slot1 slot2)) nil)
            ((and (eql x max-x) (eql y max-y)) T)
            ((eql x max-x) (equal-array-pos array1 array2 0 (+ y 1) max-x max-y))
            (T (equal-array-pos array1 array2 (+ x 1) y max-x max-y)))))

(defun equal-array (array1 array2)
    "Checks if 2 arrays are equal."
    (let ((dimensions1 (array-dimensions array1))
          (dimensions2 (array-dimensions array2)))
      (and (equal dimensions1 dimensions2)
           (equal-array-pos array1 array2 0 0 (- (first dimensions1) 1) (- (second dimensions2) 1)))))

(defun get-cycler (array)
    "Returns a function that when called cycles between the copied array elements and returns them."
    (let ((cycleElements (copy-array array))
          (currentElement -1))
        (lambda () 
            (cond((eql currentElement (- (length cycleElements) 1)) (setf currentElement 0))
                  (t (incf currentElement)))
            (aref cycleElements currentElement))))
    

;;;;; RETIRADO NA NET - AINDA TENHO QUE PERCEBER E VER SE FUNCIONA - https://stackoverflow.com/questions/14758218/two-element-combinations-of-the-elements-of-a-list-inside-lisp-without-duplicat

(defun pair-with (elem lst)
  (mapcar (lambda (a) (list elem a)) lst))

(defun unique-pairs (lst)
  (mapcon (lambda (rest) (pair-with (car rest) (cdr rest)))
          (remove-duplicates lst)))
