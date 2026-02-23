# Create a Named Empty List

Produces a list with `NULL` for each element named according to
`element_names`.

## Usage

``` r
named_null_list(element_names = NULL)
```

## Arguments

- element_names:

  `character` vector of names for the elements in the list.

## Value

`list` with names `element_names` and values `NULL`.

## See also

Other utilities:
[`file_ext()`](https://weecology.github.io/portalcasting/reference/file_ext.md),
[`foy()`](https://weecology.github.io/portalcasting/reference/foy.md),
[`ifnull()`](https://weecology.github.io/portalcasting/reference/ifnull.md),
[`messages`](https://weecology.github.io/portalcasting/reference/messages.md),
[`package_version_finder()`](https://weecology.github.io/portalcasting/reference/package_version_finder.md),
[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md),
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)

## Examples

``` r
   named_null_list(c("a", "b", "c"))
```
