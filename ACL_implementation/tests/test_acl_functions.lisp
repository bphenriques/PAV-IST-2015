;;;; test_acl_structures.lisp
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TEST MONADIC FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-monadic-.-
	; s simetric
    (assert-equalp (s -5) 
    			   (.- (s 5)))
    ; v simetric
    (assert-equalp (v -5 3 -2) 
    			   (.- (v 5 -3 2)))
    )


(define-test test-monadic-./
	; s inverse
    (assert-equalp (s 1/5) 
    			   (./ (s 5)))
    ; v inverse
    (assert-equalp (v 1/5 1/3 1/2) 
    			   (./ (v 5 3 2)))
    )

(define-test test-.!
	; s simetric
    (assert-equalp (s 6) 
    			   (.! (s 3)))
    ; v simetric
    (assert-equalp (v 1 2 6 24) 
    			   (.! (v 1 2 3 4)))
    )


(defun DtR (d) (* pi (/ d 180.0)))

(define-test test-.sin
	; s sin
    (assert-equalp (s 0) 
    			   (.sin (s 0)))
    ; v sin
    (assert-equalp (v 0 1 -1) 
    			   (.sin (v 0 (Dtr 90) (Dtr -90))))
    )

(define-test test-.cos
	; s cos
    (assert-equalp (s 1) 
    			   (.cos (s 0)))
    ; v cos
    (assert-equalp (v 1 -1) 
    			   (.cos (v 0 (Dtr 180))))
    )

(define-test test-.not
	; s not - true
    (assert-equalp (s 0) 
    			   (.not (s 200)))
    ; s not - false
    (assert-equalp (s 1) 
    			   (.not (s 0)))
    ; v not
    (assert-equalp (v 1 0 1 0) 
    			   (.not (v 0 20 0.0 -10)))
    )

(define-test test-shape
	(assert-equalp (v 0) 
				   (shape (s 200)))
	(assert-equalp (v 3) 
				   (shape (v 1 2 3)))
	(assert-equalp (v 2 3) 
				   (shape (reshape (v 2 3) (v 1 2 3 4 5 6))))
	(assert-equalp (v 2) 
				   (shape (shape (reshape (v 2 3) (v 1 2 3 4 5 6)))))
	(assert-equalp (v 3 3) 
				   (shape  (reshape (v 3 3) (interval 6))))
	)

(define-test test-interval
	(assert-equalp (v 1 2 3 4 5 6) (interval 6))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TEST DYADIC FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-.+
    (assert-equalp (v 3 4 5) 
    			   (.+ (v 2 2 2) (v 1 2 3)))
    )

(define-test test-dyadic-.-
	;test v - v with
    (assert-equalp (v 1 0 -1) 
    			   (.- (v 2 2 2) (v 1 2 3)))
    
    (assert-equalp (v -0.5 3.5 -5) 
    			   (.- (v 0.5 1.5 -2) (v 1 -2 3)))
    
    ;test v .- s
    (assert-equalp (v 3 2 1) 
    			   (.- (v 4 3 2) (s 1)))
    ;test s .- v
    (assert-equalp (v 1 2 3) 
    			   (.- (s 5) (v 4 3 2)))
	)

(define-test test-.*
	; s with s
	(assert-equalp (s 4) 
				   (.* (s 2) (s 2)))
	; s with v
    (assert-equalp (v 2 4 6) 
    			   (.* (s 2) (v 1 2 3)))
    ; v with s
    (assert-equalp (v 2 4 6) 
    			   (.* (v 1 2 3) (s 2)))
    ; v with v
    (assert-equalp (v 2 4 6) 
    			   (.* (v 2 2 2) (v 1 2 3)))
    ; special test
    (assert-equalp (v 0 0 0) 
    			   (.* (s 0) (v 1 2 3)))
    )

(define-test test-dyadic-./
	;test v v
    (assert-equalp (v 1 1/2 1/3) 
    			   (./ (v 1 1 1) (v 1 2 3)))
    ;test v s
    (assert-equalp (v 2 3/2 1) 
    			   (./ (v 4 3 2) (s 2)))
    ;test s v
    (assert-equalp (v 5/4 5/3 5/2)
    			   (./ (s 5) (v 4 3 2)))
	)

(define-test test-.//
	;test v v
    (assert-equalp (v 1 2 1) 
    			   (.// (v 1 5 9) (v 1 2 5)))
    ;test v s
    (assert-equalp (v 2 2 1) 
    			   (.// (v 5 4 3) (s 2)))
    ;test s v
    (assert-equalp (v 2 1) 
    			   (.// (s 5) (v 2 3)))
	)

(define-test test-.%
	;test v v
    (assert-equalp (v 0 1 4) 
    			   (.% (v 1 5 9) (v 1 2 5)))
    ;test v s
    (assert-equalp (v 1 0 1) 
    			   (.% (v 5 4 3) (s 2)))
    ;test s v
    (assert-equalp (v 1 2) 
    			   (.% (s 5) (v 2 3)))
	)

(define-test test-.<
	; s with v
	(assert-equalp (v 1 0) 
				   (.< (s 5) (v 9 0)))
	;v with s
	(assert-equalp (v 0 1 0) 
				   (.< (v 4 0 190) (s 4)))
	;s with s
	(assert-equalp (s 1) 
				   (.< (s 4) (s 9)))
	;v with v
	(assert-equalp (v 1 0 0) 
				   (.< (v 1 2 3) (s 2)))
	)

(define-test test-.<=
	; s with v
	(assert-equalp (v 1 0) 
				   (.<= (s 5) (v 9 0)))
	;v with s
	(assert-equalp (v 1 1 0) 
				   (.<= (v 4 0 190) (s 4)))
	;s with s
	(assert-equalp (s 1) 
				   (.<= (s 4) (s 9)))
	;v with v
	(assert-equalp (v 1 1 0) 
				   (.<= (v 1 2 3) (s 2)))
	)

(define-test test-.>
	; s with v
	(assert-equalp (v 0 1) 
				   (.> (s 5) (v 9 0)))
	;v with s
	(assert-equalp (v 0 0 1) 
				   (.> (v 4 0 190) (s 4)))
	;s with s
	(assert-equalp (s 0) 
				   (.> (s 4) (s 9)))
	;v with v
	(assert-equalp (v 0 0 1) 
				   (.> (v 1 2 3) (s 2)))
	)

(define-test test-.>=
	; s with v
	(assert-equalp (v 0 1) 
				   (.>= (s 5) (v 9 0)))
	;v with s
	(assert-equalp (v 1 0 1) 
				   (.>= (v 4 0 190) (s 4)))
	;s with s
	(assert-equalp (s 0) 
				   (.>= (s 4) (s 9)))
	;v with v
	(assert-equalp (v 0 1 1) 
				   (.>= (v 1 2 3) (s 2)))
	)

(define-test test-.=
	; s with v
	(assert-equalp (v 0 1 1) 
				   (.= (s 5) (v 3 5 5.0)))
	;v with s
	(assert-equalp (v 1 1 0) 
				   (.= (v 4 4 190) (s 4)))
	;s with s
	(assert-equalp (s 0) 
				   (.= (s 4) (s 9)))
	;v with v
	(assert-equalp (v 1 0 0) 
				   (.= (v 2 3 4) (s 2)))
	)


(define-test test-.or
    (assert-equalp  (s 0) (.or (s 0)
                               (s 0)))
    (assert-equalp  (s 1) (.or (s 0)
                               (s 1)))
    (assert-equalp  (s 1) (.or (s 1)
                               (s 0)))
    (assert-equalp  (s 1) (.or (s 1)
                               (s 1)))
    (assert-equalp  (v 1 0 0 1 1 0) (.or (s 0)
                                         (v 1 0 0 1 1 0)))
    (assert-equalp  (v 1 1 1 1 1 1) (.or (s 1)
                                         (v 1 0 0 1 1 0)))
    (assert-equalp  (v 1 1 1 1 1 1 1 1 1) (.or (v 1 1 1 1 0 1 1 1 1)
                                               (v 0 1 0 0 1 1 0 1 1))))

(define-test test-.and
    (assert-equalp  (s 0) (.and (s 0)
                                (s 0)))
    (assert-equalp  (s 0) (.and (s 0)
                                (s 1)))
    (assert-equalp  (s 0) (.and (s 1)
                                (s 0)))
    (assert-equalp  (s 1) (.and (s 1)
                                (s 1)))
    (assert-equalp  (v 0 0 0 0 0 0) (.and (s 0)
                                          (v 1 0 0 1 1 0)))
    (assert-equalp  (v 1 0 0 1 1 0) (.and (s 1)
                                          (v 1 0 0 1 1 0)))
    (assert-equalp  (v 0 1 0 0 0 1 0 1 1) (.and (v 1 1 1 1 0 1 1 1 1)
                                                (v 0 1 0 0 1 1 0 1 1))))

(define-test test-.drop
	(assert-equalp  (v 3 4 5 6 7 8 9 10) 
					(drop (s 2) (interval 10)))

	(assert-equalp  (v 1 2 3 4 5 6 7 8) 
					(drop (s -2) (interval 10)))
	)

(define-test test-.catenate
	(assert-equalp (v 1 2 3 4 5)  
				   (catenate (v 1 2) (v 3 4 5)))
	(assert-equalp (reshape (v 2 4) (v 1 2 5 6 3 4 7 8))
                   (catenate (reshape (v 2 2) (v 1 2 3 4))
                             (reshape (v 2 2) (v 5 6 7 8))))
 	(assert-equalp (reshape (v 2 2 4) (v 1 2 3))
                   (catenate (reshape (v 2 2 2) (v 1 2 2 3 3 1 1 2))
                             (reshape (v 2 2 2) (v 3 1 1 2 2 3 3 1)))))


(define-test test-.member?
	(assert-equalp (reshape (v 3 3) (v 1 1 0 0 1 1 0 0 1)) 
				   (member? (reshape (v 3 3) (interval 4)) (v 1 2)))
	)

(define-test test-.select
	(assert-equalp (v 4 6 9 30) 
				   (select (v 0 1 0 0 1 1 1) 
				   		   (v 99 4 1 2 6 9 30)))
	(assert-equalp (v 6 7 5 4)  
				   (let ((v (v 1 6 2 7 3 0 5 4)))
						(select (.> v (s 3)) v)))
	(assert-equalp (reshape (v 2 2) (v 1 3 4 6)) 
				   (select (v 1 0 1) (reshape (v 2 3) (interval 6))))
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TEST MONADIC OPERATORS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-test test-fold
	(assert-equalp (s 10) 
				   (funcall (fold #'.+) (v 1 2 3 4)))
	(assert-equalp (s 24) 
				   (funcall (fold #'.*) (v 1 2 3 4)))
	)

(define-test test-scan
	(assert-equalp (v 1 3 6 10) 
				   (funcall (scan #'.+) (v 1 2 3 4)))
	(assert-equalp (v 1 2 6 24) 
				   (funcall (scan #'.*) (v 1 2 3 4)))
	)

(define-test test-outer-product
    (assert-equalp (reshape (v 2 3 4)
                            (v 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0))
            	   (funcall (outer-product #'.=) (v 4 7) (reshape (v 3 4) (interval 12))))
    (assert-equalp (s 2)
           	       (funcall (outer-product #'.+) (s 1) (s 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TEST DIADIC OPERATORS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-inner-product
	(assert-equalp (reshape (v 2 3) (v 90 120 150 190 260 330))
           	       (funcall (inner-product #'.+ #'.*)
                            (reshape (v 2 2) (v 10 20 30 40))
                            (reshape (v 2 3) (v 1 2 3 4 5 6))))
    (assert-equalp (s 2)
                   (funcall (inner-product #'.* #'.+)
                            (s 1)
                            (s 1)))
    (assert-equalp (v 8 12 20)
                   (funcall (inner-product #'.+ #'.*)
                            (s 4)
                            (v 2 3 5)))
    (assert-equalp (reshape (v 2 2) (v -17 -20 -9 -12))
                   (funcall (inner-product #'.+ #'.-)
                            (reshape (v 2 3) (v 1 2 1 3 4 5))
                            (reshape (v 3 2) (v 5 6 7 8 9 10)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TEST EXERCISES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-tally
	(assert-equalp (s 18) 
				   (tally (reshape (v 3 3 2) (interval 5))))
	(assert-equalp (s 24)
				   (tally (reshape (v 1 2 3 4) (interval 5))))

	(assert-equalp (s 1)
				   (tally (s 99)))

	(assert-equalp (s 5)
				   (tally (v -1 -2 -3 -4 -5)))
	)

(define-test test-rank
	(assert-equalp (s 3) 
				   (rank (reshape (v 4 5 2) (interval 5))))
	(assert-equalp (s 2) 
				   (rank (reshape (v 4 5) (interval 5))))

	(assert-equalp (s 0)
				   (rank (s 10)))

	(assert-equalp (s 1)
				   (rank (v 1 2 3 4)))
	)

(define-test test-within
	; within only has as first argument a vector
	(assert-equalp (v 7 8 6 5) 
				   (within (v 2 7 3 1 9 8 4 6 5) (s 5) (s 8)))
	)

(define-test test-ravel
	(assert-equalp (v 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2 3 4) 
				   (ravel (reshape (v 2 3 4) (interval 10))))
	(assert-equalp (v 10)
				   (ravel (s 10)))

	(assert-equalp (v 1 2 3 4 5)
				   (ravel (interval 5)))
	)

(define-test test-prime
	(assert-equalp (v 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) 
				   (prime (s 50)))
	)