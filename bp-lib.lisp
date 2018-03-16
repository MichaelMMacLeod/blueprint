(in-package :cl-user)

(load "args.lisp")

(defpackage :macleod.bp-lib
  (:use :common-lisp
        :macleod.args)
  (:export :make-parser
           :set-token
           :get-token
           :list-tokens
           :make-parser-path
           :write-parser
           :load-parser
           :make-parser
           :read-token
           :parse-standard-input
           :--make-parser
           :--parse-token
           :--set-token
           :--list-tokens
           :--run
           :--make-executable))

(in-package :macleod.bp-lib)

(defun set-token (token value parser)
  "Sets a key in parser to a value."
  (setf (gethash token parser) value))

(defun get-token (token parser)
  "Returns the value of a key in parser."
  (gethash token parser))

(defun list-tokens (parser)
  "Lists the keys and values in parser."
  (loop for k being the hash-keys in parser using (hash-value v)
        do (format t "~a: ~a~%" k v)))

(defun make-parser-path (parser-name)
  "Returns the full file path to a parser given a name."
  (format nil "~~/.config/blueprint/parsers/~a" parser-name))

(defun write-parser (parser parser-name)
  "Writes a parser with a given name to a file."
  (let ((parser-path (make-parser-path parser-name)))
    (ensure-directories-exist parser-path)
    (with-open-file (out parser-path
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (print parser out)))))

(defun load-parser (parser-name)
  "Reads the parser with the given name."
  (with-open-file (in (make-parser-path parser-name))
    (with-standard-io-syntax
      (read in))))

(defun make-parser (parser-name)
  "Creates a hash table with string equality and writes it to a file."
  (write-parser (make-hash-table :test 'equal) parser-name))

(defun read-token (parser)
  "Returns the token starting at the current character in *standard-input*.
The stream should start at a '[', as in '[token-name]'. read-token will
then consume '[token-name]' and return the value of the token 'token-name'
in the parser (or '', if there is no key 'token-name' in parser)."
  (read-char)
  (loop for curr = (read-char *standard-input* nil)
        if (char= #\] curr)
          do (return (coerce (get-token (coerce xs 'string) parser) 'list))
        else
          collect curr into xs))

(defun parse-standard-input (parser)
  "Consumes *standard-input* and returns it with all #[tokens] replaced
with their corresponding values in parser."
  (loop for curr = (read-char *standard-input* nil)
        for next = (peek-char nil *standard-input* nil)
        if (null curr)
          return (coerce xs 'string)
        else if (and (char= #\# curr)
                     (char= #\[ next))
          append (read-token parser) into xs
        else
          collect curr into xs))

(defun --make-parser ()
  "Runs make-parser if the proper commandline options are supplied."
  (with-args ("--make-parser" parser-name)
    (make-parser parser-name)))

(defun --set-token ()
  "Runs set-token if the proper commandline options are supplied."
  (with-args (parser-name "--set-token" token value)
    (let ((parser (load-parser parser-name)))
      (set-token token value parser)
      (write-parser parser parser-name))))

(defun --list-tokens ()
  "Runs list-tokens if the proper commandline options are supplied."
  (with-args (parser-name "--list-tokens")
    (list-tokens (load-parser parser-name))))

(defun --run ()
  "Prints the output of parse-standard-input if the proper commandline 
options are supplied."
  (with-args (parser-name "--run")
    (format t "~a" (parse-standard-input (load-parser parser-name)))))

(defun --make-executable ()
  "Loads a parser, then dumps the core image to a file in the current
directory. The file is executable, and will act like --run has been
supplied."
  (with-args (parser-name "--make-executable")
    (let ((parser (load-parser parser-name)))
      (sb-ext:save-lisp-and-die 
        parser-name
        :toplevel #'(lambda () (format t "~a" (parse-standard-input parser)))
        :executable t))))
