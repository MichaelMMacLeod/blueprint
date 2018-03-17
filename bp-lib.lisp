(in-package :cl-user)

(load "args.lisp")

(defpackage :macleod.bp-lib
  (:use :common-lisp
        :macleod.args)
  (:export :make-parser
           :list-parsers
           :set-token
           :get-token
           :list-tokens
           :make-parser-path
           :write-parser
           :load-parser
           :make-parser
           :read-token
           :parse-standard-input
           :--help
           :--make-parser
           :--list-parsers
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

(defun make-parser (parser-name)
  "Creates a hash table with string equality and writes it to a file."
  (write-parser (make-hash-table :test 'equal) parser-name))

(defun load-parser (parser-name)
  "Reads the parser with the given name from a file."
  (with-open-file (in (make-parser-path parser-name))
    (with-standard-io-syntax
      (read in))))

(defun list-parsers ()
  "Lists all (non-executable) parser."
  (ensure-directories-exist "~/.config/blueprint/parsers/")
  (let ((parsers (mapcar #'pathname-name 
                         (directory "~/.config/blueprint/parsers/*"))))
    (format t "~{~a~%~}" parsers)))

(defun read-token (parser) "Returns the token starting at the current character in *standard-input*.
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

(defun --help ()
  "Displays commandline option help."
  (princ
"Usage: bp --make-parser PARSER
       bp --list-parsers
       bp PARSER [PARSER-OPTION ..]

Create and run file parsers.

  --make-parser PARSER            Create a new parser. Parsers are located in
                                  ~/.config/blueprint/parsers/.

  --list-parsers                  Lists all parsers. This does not include
                                  standalone parsers created with
                                  --make-executable.

PARSER-OPTION

  PARSER --set-token TOKEN VALUE  Make #[TOKEN] parse to VALUE.

  PARSER --list-tokens            List all tokens.

  PARSER --run                    Read from standard input until EOF, parsing
                                  it.
                                    $ ./bp my-parser --list-tokens
                                    my-token: world
                                    $ cat parse-me
                                    Hello, #[my-token]!
                                    $ cat parse-me | ./bp my-parser --run
                                    Hello, world!

  PARSER --make-executable        Create a standalone executable parser.
                                    $ cat parse-me
                                    Hello, #[my-token]!
                                    $ cat parse-me | ./bp my-parser --run
                                    Hello, world
                                    $ ./bp my-parser --make-executable
                                    $ cat parse-me | ./my-parser
                                  The executable does not need to be supplied
                                  --run to parse its input.
"))

(defun --make-parser ()
  "Runs make-parser if the proper commandline options are supplied."
  (with-args ("--make-parser" parser-name)
    (make-parser parser-name)))

(defun --list-parsers ()
  "Runs list-parsers if the proper commandline options are supplied."
  (with-args ("--list-parsers")
    (list-parsers)))

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
directory if the proper commandline options are supplied. The file is 
executable, and will act like --run has been supplied."
  (with-args (parser-name "--make-executable")
    (let ((parser (load-parser parser-name)))
      (sb-ext:save-lisp-and-die 
        parser-name
        :toplevel #'(lambda () (format t "~a" (parse-standard-input parser)))
        :executable t))))
