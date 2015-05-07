;;;; test_acl_structures.lisp
;;;;
(defun assert-tensor-equal (tensor)
    )

(define-test test-.+
    (assert-equalp (v 3 4 5) (.+ (v 2 2 2) (v 1 2 3))))


(define-test test-.*
    (assert-equalp (v 2 4 6) (.* (s 2) (v 1 2 3))))


(define-test test-.-
    (assert-equalp (v 1 0 -1) (.- (v 2 2 2) (v 1 2 3)))
    (assert-equalp (v -0.5 3.5 -5) (.- (v 0.5 1.5 -2) (v 1 -2 3))))
