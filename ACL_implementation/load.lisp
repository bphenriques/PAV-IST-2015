;;;; load.lisp
;;;;
;;;; Main APL implementation load file.
;;;;
;;;; Loads all lisp files needed to run an APL implementation on top of
;;;; LISP.
;;;;
;;;; Made by group 5:
;;;;    72913 - Bruno Henriques
;;;;    72960 - Tiago Santos
;;;;    73378 - Nuno Xu
;;;;
;;;; Created for PAV APL project.

(defconstant +source-folder-location+ "src/"
    "Location of main source folder")

(defun load-compile-file (file-name)
    ;(print "---------------------------------")
    ;(print (concatenate 'string "LOADING: " file-name))
    (load (concatenate 'string +source-folder-location+ file-name)
        :verbose nil))
    ;(print "------         DONE         -----")
    ;(print "---------------------------------"))

(load-compile-file "apl_util.lisp")
(load-compile-file "apl_bool.lisp")
(load-compile-file "apl_structures.lisp")
(load-compile-file "apl_structures_functions.lisp")
(load-compile-file "apl_functions.lisp")
(load-compile-file "apl_monadic_functions.lisp")
(load-compile-file "apl_dyadic_functions.lisp")
(load-compile-file "apl_monadic_operators.lisp")
(load-compile-file "apl_dyadic_operators.lisp")
(load-compile-file "apl_exercises.lisp")
(load-compile-file "apl_extensions.lisp")
