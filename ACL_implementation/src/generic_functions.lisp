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

(defun integer-division (n1 n2)
	(nth-value 0 (floor n1 n2)))

(defun remainder-integer-division (n1 n2)
	(nth-value 1 (floor n1 n2)))
	
(defun get-member-finder (member-vector)
	(let ((members member-vector))
		(lambda (n) 
			(create-bool (find n members)))))
			
			
