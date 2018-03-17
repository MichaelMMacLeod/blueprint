# blueprint

Create parsers which read from standard input and replace all #[TOKENS] with VALUES

```
Usage: bp --make-parser PARSER
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
```
