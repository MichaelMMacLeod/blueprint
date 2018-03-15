# blueprint

Reads from standard input, replaces all #[tokens] with a pre-defined string.

```
$ ./blueprint --create-parser colorize
$ ./blueprint colorize --add-token background-color 000000
$ ./blueprint colorize --list-tokens
background-color: 000000
$ cat .Xresources
*.background: "##[background-color]";
$ cat .Xresources | ./blueprint --run colorize
*.background: "#000000";
```
