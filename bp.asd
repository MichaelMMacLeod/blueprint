(in-package :cl-user)

(defpackage :macleod.bp-system
  (:use :common-lisp :asdf))

(in-package :macleod.bp-system)

(defsystem "bp"
           :description "bp - parse file blueprints"
           :version "0.1"
           :author "Michael MacLeod <michaelmmacleod@gmail.com>"
           :license "MIT"
           :depends-on (:args) ; https://github.com/michaelmmacleod/args.git
           :components ((:file "bp")))
