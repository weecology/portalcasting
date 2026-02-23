# Prepare Lunar Data for the Portalcasting Repository

Get time information (calendar dates, census periods, and newmoon
numbers) associated with trapping events (achieved and missed) based on
a lunar survey schedule.  
`add_forecast_newmoons` adds future newmoon dates to the newmoon table
from `prepare_newmoons` associated with the forecast.

## Usage

``` r
prepare_newmoons(main = ".")

add_forecast_newmoons(main = ".", newmoons = NULL)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- newmoons:

  `data.frame` of newmoon data.

## Value

Some version of a newmoons `data.frame`.  
`prepare_newmoons`: fully appended and formatted `data.frame` (also
saved out if `settings$save = TRUE`).  
`add_forecast_newmoons`: fully appended and formatted `data.frame`.

## See also

Content preparation functions:
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`prepare covariates`](https://weecology.github.io/portalcasting/reference/prepare-covariates.md),
[`prepare metadata`](https://weecology.github.io/portalcasting/reference/prepare-metadata.md),
[`prepare models`](https://weecology.github.io/portalcasting/reference/prepare-models.md),
[`prepare rodents`](https://weecology.github.io/portalcasting/reference/prepare-rodents.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "newmoons")

   create_dir(main = main1)
   fill_resources(main = main1)
   fill_forecasts(main = main1)
   fill_fits(main = main1)
   fill_models(main = main1)

   prepare_newmoons(main = main1)

   unlink(main1, recursive = TRUE)
} # }
```
