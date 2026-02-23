# Update a List's Elements

Update a list with new values for elements

## Usage

``` r
update_list(list = list(), ...)
```

## Arguments

- list:

  `list` to be updated with `...`.

- ...:

  Named elements to update in `list`

## Value

Updated `list`.

## See also

Other utilities:
[`file_ext()`](https://weecology.github.io/portalcasting/reference/file_ext.md),
[`foy()`](https://weecology.github.io/portalcasting/reference/foy.md),
[`ifnull()`](https://weecology.github.io/portalcasting/reference/ifnull.md),
[`messages`](https://weecology.github.io/portalcasting/reference/messages.md),
[`named_null_list()`](https://weecology.github.io/portalcasting/reference/named_null_list.md),
[`package_version_finder()`](https://weecology.github.io/portalcasting/reference/package_version_finder.md),
[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md)

## Examples

``` r
   orig_list <- list(a = 1, b = 3, c = 4)
   update_list(orig_list)
   update_list(orig_list, a = "a")
   update_list(orig_list, a = 10, b = NULL)
```
