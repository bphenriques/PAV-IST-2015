;;;; test_acl_structures.lisp
;;;;
(defun assert-tensor-equal (tensor)
    )

(define-test test-.+
    (assert-equalp (v 3 4 5) (.+ (v 2 2 2) (v 1 2 3))))


(define-test test-.*
    (assert-equalp (v 2 4 6) (.* (s 2) (v 1 2 3))))
