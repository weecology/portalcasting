# Replace a Value with an Alternative if it is NULL

Replaces the focal input with the alternative value if it is `NULL`.

## Usage

``` r
ifnull(x = NULL, alt = NULL)
```

## Arguments

- x:

  Focal input.

- alt:

  Alternative value.

## Value

`x` if not `NULL`, `alt` otherwise.

## See also

Other utilities:
[`file_ext()`](https://weecology.github.io/portalcasting/reference/file_ext.md),
[`foy()`](https://weecology.github.io/portalcasting/reference/foy.md),
[`messages`](https://weecology.github.io/portalcasting/reference/messages.md),
[`named_null_list()`](https://weecology.github.io/portalcasting/reference/named_null_list.md),
[`package_version_finder()`](https://weecology.github.io/portalcasting/reference/package_version_finder.md),
[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md),
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)

## Examples

``` r
   ifnull(NULL, 123)
   ifnull(TRUE, 123)
   ifnull(FALSE, 123)
```
