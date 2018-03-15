; https://github.com/michaelmmacleod/args.git

(in-package :cl-user)

(defpackage :macleod.args
  (:use :common-lisp)
  (:export :with-args))

(in-package :macleod.args)

;;; Matches args against args-match, binding symbols when appropriate.
;;;
;;; (with-matches '("--help" "my-command")
;;;                ("--help" command-name)
;;;   (print command-name)) ; prints "my-command"
;;;
;;; nil if the lists do not line up or match properly.
(defmacro with-matches (args args-match &body body)
  (if (= (length (eval args)) (length args-match))
    (block outer
      `(let ,(loop with matches = 0
                   for arg in (eval args)
                   for arg-match in args-match
                   if (symbolp arg-match)
                     collect `(,arg-match ,arg) into xs and
                     do (incf matches)
                   do (and (stringp arg-match) (string= arg-match arg)
                           (incf matches))
                   finally (if (= matches (length args-match))
                             (return xs)
                             (return-from outer nil)))
         ,@body))))

;;; Automatically supplies command line arguments to with-matches
(defmacro with-args (args-match &body body)
  `(with-matches (cdr sb-ext:*posix-argv*) ,args-match
     ,@body))
