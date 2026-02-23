# Calculate the Fraction of the Year from a Date

Based on the year in which the date occurred, determine the fraction of
the year (foy) for the date (in relation to New Year's Eve in that
year).

## Usage

``` r
foy(dates = NULL)
```

## Arguments

- dates:

  `Date`(s) or `Date`-conformable value(s) to be converted to the
  fraction of the year.

## Value

`numeric` value(s) of the fraction of the year.

## See also

Other utilities:
[`file_ext()`](https://weecology.github.io/portalcasting/reference/file_ext.md),
[`ifnull()`](https://weecology.github.io/portalcasting/reference/ifnull.md),
[`messages`](https://weecology.github.io/portalcasting/reference/messages.md),
[`named_null_list()`](https://weecology.github.io/portalcasting/reference/named_null_list.md),
[`package_version_finder()`](https://weecology.github.io/portalcasting/reference/package_version_finder.md),
[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md),
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)

## Examples

``` r
   Sys.Date( )
   foy(Sys.Date())
```
