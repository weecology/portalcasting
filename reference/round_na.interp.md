# Round an Interpolated Series

Wraps [`round`](https://rdrr.io/r/base/Round.html) around
[`forecast::na.interp`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html)
to provide a rounded interpolated series, which is then enforced to be
greater than or equal to a minimum value (default `min_val = 0`) via
[`pmax`](https://rdrr.io/r/base/Extremes.html).

## Usage

``` r
round_na.interp(
  x,
  lambda = NULL,
  linear = (frequency(x) <= 1 | sum(!is.na(x)) <= 2 * frequency(x)),
  digits = 0,
  min_val = 0
)
```

## Arguments

- x:

  A time series passed directly to
  [`forecast::na.interp`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html).

- lambda:

  Box-Cox transformation parameter passed directly to
  [`forecast::na.interp`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html).

- linear:

  `logical` indicator of if linear interpolation should be used. Passed
  directly to
  [`forecast::na.interp`](https://pkg.robjhyndman.com/forecast/reference/na.interp.html).

- digits:

  `integer` or `numeric` integer of how many digits to round to. Passed
  directly to [`round`](https://rdrr.io/r/base/Round.html).

- min_val:

  `integer` or `numeric` integer of minimum value allowable in the
  series.

## Value

`numeric` series.

## See also

Other utilities:
[`file_ext()`](https://weecology.github.io/portalcasting/reference/file_ext.md),
[`foy()`](https://weecology.github.io/portalcasting/reference/foy.md),
[`ifnull()`](https://weecology.github.io/portalcasting/reference/ifnull.md),
[`messages`](https://weecology.github.io/portalcasting/reference/messages.md),
[`named_null_list()`](https://weecology.github.io/portalcasting/reference/named_null_list.md),
[`package_version_finder()`](https://weecology.github.io/portalcasting/reference/package_version_finder.md),
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)

## Examples

``` r
   round_na.interp(x = c(1, 2, 3, NA, NA, 170))
   round_na.interp(x = c(-1, 2, 3, NA, NA, 170), min_val = 1)
```
