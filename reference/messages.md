# Functions for Message Generation

Create messages for use in the portalcasting pipeline.  
  
`messageq`: Optionally generate a message based on a logical input. Uses
a wrapper on [`message`](https://rdrr.io/r/base/message.html) that,
given the input to `quiet`, generates the message(s) in `...` or not.  
`break_line`: Creates a horizontal line of characters ending with a
newline call for messages.  
`break_lines`: Creates a set of horizontal line of characters ending
with a newline call for messages.  
`castle`: Creates a text drawing of a sandcastle of characters for
messages.

## Usage

``` r
messageq(..., quiet = FALSE, domain = NULL, appendLF = TRUE)

break_line(char = "-", reps = 60)

break_lines(nlines = 2, char = "-", reps = 60)

castle()
```

## Arguments

- ...:

  zero or more objects that can be coerced to `character` and are
  concatenated with no separator added, or a single condition object.
  See [`message`](https://rdrr.io/r/base/message.html).

- quiet:

  `logical` indicator if the message should be generated.

- domain:

  The domain for the translation. If `NA`, messages will not be
  translated. See [`message`](https://rdrr.io/r/base/message.html) and
  [`base::gettext`](https://rdrr.io/r/base/gettext.html).

- appendLF:

  `logical` indicator if messages given as a `character` string should
  have a newline appended. See
  [`message`](https://rdrr.io/r/base/message.html).

- char:

  `character` value to repeated `reps` times to form the break line.

- reps:

  `integer`-conformable value for number of times `char` is replicated.

- nlines:

  `integer`-conformable value for the number of `break_line`s to
  include. Defaults to `2` lines.

## Value

`messageq`: A message is given, and `NULL` returned,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.  
`break_line`: The `character` of the line to be passed to
[`message`](https://rdrr.io/r/base/message.html) via `messageq`.  
`break_lines`: The `character` of the lines to be passed to
[`message`](https://rdrr.io/r/base/message.html) via `messageq`.  
`castle`: The `character` of the sandcastle to be passed to
[`message`](https://rdrr.io/r/base/message.html) via `messageq`.

## See also

Other utilities:
[`file_ext()`](https://weecology.github.io/portalcasting/reference/file_ext.md),
[`foy()`](https://weecology.github.io/portalcasting/reference/foy.md),
[`ifnull()`](https://weecology.github.io/portalcasting/reference/ifnull.md),
[`named_null_list()`](https://weecology.github.io/portalcasting/reference/named_null_list.md),
[`package_version_finder()`](https://weecology.github.io/portalcasting/reference/package_version_finder.md),
[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md),
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)

## Examples

``` r
   messageq("hi ", "how are you?")
   messageq("hi ", "how are you?", quiet = TRUE)
   break_line( )
   break_lines( )
   castle( )
```
