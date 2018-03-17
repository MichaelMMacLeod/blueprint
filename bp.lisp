(in-package :cl-user)

(defpackage :macleod.bp
  (:use :common-lisp
        :macleod.bp-lib))

(in-package :macleod.bp)

(defun read-args ()
  (--run)
  (--list-parsers)
  (--list-tokens)
  (--set-token)
  (--make-parser)
  (--make-executable)
  (--help))

(read-args)
