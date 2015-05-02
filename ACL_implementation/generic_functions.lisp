(defun fact (n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(defun simetric (n)
	(- 0 n))

(defun inverse (n)
	(if (= n 0)
		0
		(/ 1 n)))