# pandoc-mario

`pandoc` is most often used to convert natural language documents between
formats. It does this by first parsing the input document into a general AST
and then printing the AST into the output format. The purpose of this
`pandoc-mario` package is to play with operations just on the AST. For example,
a format-agnostic document search function.

The internal AST can be printed as a Haskell structure with:

```
pandoc -t native <INPUT_FILE>
```

The native output is informative, but cannot easily be used directly. More useful is the JSON representation of the AST:

```
pandoc -t json <INPUT_FILE>
```

The tools I will make in this package will all read JSON from STDIN, for
example:

```
pandoc -t json <INPUT_FILE> | <MY_TOOL>
```

If I wanted to create a filter, I could write:

```
pandoc -t json <INPUT_FILE> | <MY_TOOL> | pandoc -f json -t <OUTPUT_FORMAT>
```
