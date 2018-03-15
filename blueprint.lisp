(in-package :cl-user)

(load "args.lisp")

(defpackage :macleod.blueprint
  (:use :common-lisp
        :macleod.args))

(in-package :macleod.blueprint)

(defparameter config nil)

(defun make-config ()
  (setf config (make-hash-table :test 'equal)))

(defun add-token (token value)
  (setf (gethash token config) value))

(defun get-token (token)
  (gethash token config))

(defun write-config (name)
  (ensure-directories-exist "~/.config/bpt/")
  (with-open-file (out (format nil "~~/.config/bpt/~a" name)
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print config out))))

(defun load-config (name)
  (with-open-file (in (format nil "~~/.config/bpt/~a" name))
    (with-standard-io-syntax
      (setf config (read in)))))

(defun create-parser (config-file)
  (make-config)
  (write-config config-file))

(defun list-tokens ()
  (loop for k being the hash-keys in config using (hash-value v)
        do (format t "~a: ~a~%" k v)))

(defun read-token ()
  (read-char)
  (loop for curr = (read-char t nil)
        if (char= #\] curr)
          do (return (coerce (get-token (coerce xs 'string)) 'list))
        else
          collect curr into xs))

(defun parse-standard-input ()
  (loop for curr = (read-char t nil)
        for next = (peek-char nil t nil)
        if (null curr)
          return (coerce xs 'string)
        else if (and (char= #\# curr)
                     (char= #\[ next))
          append (read-token) into xs
        else
          collect curr into xs))

(defun run-args ()
  (with-args ("--create-parser" parser-name)
    (make-config)
    (write-config parser-name))
  (with-args (parser-name "--add-token" token value)
    (load-config parser-name)
    (add-token token value)
    (write-config parser-name))
  (with-args (parser-name "--list-tokens")
    (load-config parser-name)
    (list-tokens))
  (with-args ("--run" parser-name)
    (load-config parser-name)
    (format t "~a" (parse-standard-input))))

(run-args)
