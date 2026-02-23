# Determine a File's Extension

Based on the separating character, determine the file extension.

## Usage

``` r
file_ext(path, sep_char = ".")
```

## Arguments

- path:

  `character` value of the file path possibly with an extension.

- sep_char:

  `character` value of the separator that delineates the extension from
  the file path.  
  Generally, this will be `."`, but for some API URLs, the extension is
  actually a query component, so the separator may sometimes need to be
  `"="`.

## Value

`character` value of the extension (`file_ext`).

## See also

Other utilities:
[`foy()`](https://weecology.github.io/portalcasting/reference/foy.md),
[`ifnull()`](https://weecology.github.io/portalcasting/reference/ifnull.md),
[`messages`](https://weecology.github.io/portalcasting/reference/messages.md),
[`named_null_list()`](https://weecology.github.io/portalcasting/reference/named_null_list.md),
[`package_version_finder()`](https://weecology.github.io/portalcasting/reference/package_version_finder.md),
[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md),
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)

## Examples

``` r
   file_ext("home/folders.with.dots/stuff/ok.csv")
   file_ext(NMME_urls()[[1]])
   file_ext(NMME_urls()[[1]], "=")
```
