;;;; test_acl_structures.lisp
;;;;

(define-test test-.+
    (assert-equalp (v 3 4 5) (.+ (v 2 2 2) (v 1 2 3)))
    )


(define-test test-.*
	; s with s
	(assert-equalp (s 4) (.* (s 2) (s 2)))
	; s with v
    (assert-equalp (v 2 4 6) (.* (s 2) (v 1 2 3)))
    ; v with s
    (assert-equalp (v 2 4 6) (.* (v 1 2 3) (s 2)))
    ; v with v
    (assert-equalp (v 2 4 6) (.* (v 2 2 2) (v 1 2 3)))
    ; special test
    (assert-equalp (v 0 0 0) (.* (s 0) (v 1 2 3)))
    )

(define-test test-monadic-.-
	; s simetric
    (assert-equalp (s -5) (.- (s 5)))
    ; v simetric 
    (assert-equalp (v -5 3 -2) (.- (v 5 -3 2)))
    )



(define-test test-dyadic-.-
	;test v - v with
    (assert-equalp (v 1 0 -1) (.- (v 2 2 2) (v 1 2 3)))
    (assert-equalp (v -0.5 3.5 -5) (.- (v 0.5 1.5 -2) (v 1 -2 3)))
    ;test v .- s
    (assert-equalp (v 3 2 1) (.- (v 4 3 2) (s 1)))
    ;test s .- v
    (assert-equalp (v 1 2 3) (.- (s 5) (v 4 3 2)))
	)

(define-test test-monadic-./
	; s inverse
    (assert-equalp (s 1/5) (./ (s 5)))
    ; v inverse 
    (assert-equalp (v 1/5 1/3 1/2) (./ (v 5 3 2)))
    )



(define-test test-dyadic-./
	;test v ./ v
    (assert-equalp (v 1 1/2 1/3) (./ (v 1 1 1) (v 1 2 3)))
    ;test v ./ s
    (assert-equalp (v 2 3/2 1) (./ (v 4 3 2) (s 2)))
    ;test s ./ v
    (assert-equalp (v 5/4 5/3 5/2) (./ (s 5) (v 4 3 2)))
	)

(define-test test-.!
	; s simetric
    (assert-equalp (s 6) (.! (s 3)))
    ; v simetric 
    (assert-equalp (v 1 2 6 24) (.! (v 1 2 3 4)))
    )

(defun DtR (d) (* pi (/ d 180.0)))

(define-test test-.sin
	; s sin
    (assert-equalp (s 0) (.sin (s 0)))
    ; v sin 
    (assert-equalp (v 0 1 -1) (.sin (v 0 (Dtr 90) (Dtr -90))))
    )

(define-test test-.sin
	; s cos
    (assert-equalp (s 1) (.cos (s 0)))
    ; v cos 
    (assert-equalp (v 1 -1) (.cos (v 0 (Dtr 180))))
    )

(define-test test-.not
	; s not - true
    (assert-equalp (s 1) (.not (s 200)))
    ; s not - false
    (assert-equalp (s 0) (.not (s 0)))
    ; v not 
    (assert-equalp (v 0 1 0 1) (.not (v 0 20 0.0 -10)))
    )

(define-test test-shape
	(assert-equalp (v 0) (shape (s 200)))
	(assert-equalp (v 3) (shape (v 1 2 3)))
	(assert-equalp (v 2 3) (shape (reshape (v 2 3) (v 1 2 3 4 5 6))))
	(assert-equalp (v 2) (shape (shape (reshape (v 2 3) (v 1 2 3 4 5 6)))))
	(assert-equalp (v 3 3) (shape  (reshape (v 3 3) (interval 6))))
	)

(define-test test-interval
	(assert-equalp (v 1 2 3 4 5 6) (interval 6))
	;TODO testar tambem com numeros negativos
)
