# Download Climate Forecasts

Downloads climate forecasts, presently only from the North American
Multi-Model Ensemble (NMME), into the `<main>/<resources>` sub.  
`download_climate_forecasts` downloads the files from the pre-defined
URLs.  
`NMME_urls` generates the URL for a specific request to the NMME API
based on parameters. See arguments for specifics and `Details` for
links.

## Usage

``` r
download_climate_forecasts(
  main = ".",
  resources_sub = "resources",
  version = Sys.Date(),
  source = "NMME",
  data = c("tasmin", "tasmean", "tasmax", "pr"),
  quiet = FALSE,
  verbose = FALSE,
  force = FALSE,
  timeout = getOption("timeout")
)

NMME_urls(
  start = Sys.Date(),
  end = as.Date("2050-01-01"),
  model = "ENSMEAN",
  lat = 31.9555,
  lon = -109.0744,
  freq = "daily",
  data = c("tasmin", "tasmean", "tasmax", "pr")
)
```

## Arguments

- main:

  `character` value defining the main component of the portalcasting
  directory tree.

- resources_sub:

  `character` value defining the resources subdirectory of the
  portalcasting directory tree.

- version:

  `Date`-coercible start of the climate forecast. See `NMME_urls` (used
  as `start`).

- source:

  `character` indicator of the source for the download. Only `"NMME"`
  presently available.

- data:

  `character` value of the type of data, one of `"tasmin"` (minimum
  temperature), `"tasmean"` (mean temperature), `"tasmax"` (maximum
  temperature), `"pr"` (precipitation), `"dps"` (dew point), `"rsds"`
  (shortwave radiation; sun intensity), `"was"` (wind speed).

- quiet:

  `logical` indicator if progress messages should be quieted.

- verbose:

  `logical` indicator if detailed messages should be generated.

- force:

  `logical` indicator of whether or not existing files or folders (such
  as the archive) should be over-written if an up-to-date copy exists
  (most users should leave as `FALSE`).

- timeout:

  Positive `integer` or integer `numeric` seconds for timeout on
  downloads. Temporarily overrides the `"timeout"` option in
  [`options`](https://rdrr.io/r/base/options.html).

- start, end:

  `Date` for the start and end of the forecast.

- model:

  `character` value of the model to use, one of `"ENSMEAN"`,
  (Multi-Model Mean), `"CMC1"` (CMC1-CanCM3), `"CMC2"` (CMC2-CanCM4),
  `"CFCSv2"` (NCEP-CFSv2), `"GFDL"` (GFDL-CM2.1), `"GFDL-FLOR"`
  (GFDL-FLOR), or `"NCAR"` (NCAR-CCSM4). Presently can only take one
  value.

- lat, lon:

  `numeric` latitude and longitude values used to downscale the model.
  Presently can only take one value for each.

- freq:

  `character` value of the frequency of the data, can be `"daily"` or
  `"XmonthAverage"`, where `"X"` is a number between `1` and `7`.
  Presently can only take one value.

## Value

`download_climate_forecasts: `NULL`, [`invisible`][base::invisible]-ly. \cr `NMME_urls`: amed `character`vector of URLs, or`NULL`if`data`, `freq`, or `model`is`NULL\`.

## Details

The [Northwest Knowledge
Network](https://www.northwestknowledge.net/home/) (NKN) at the
University of Idaho provides a [simple
API](https://climate.northwestknowledge.net/RangelandForecast/download.php)
to download downscaled climate forecasts using the [North American
Multi-Model Ensemble](http://www.cpc.ncep.noaa.gov/products/NMME/)
(NMME) set.

## See also

Other downloads:
[`download archive`](https://weecology.github.io/portalcasting/reference/download-archive.md)

## Examples

``` r
   NMME_urls( )

if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "dcf")
   create_dir(main = main1)
   download_climate_forecasts(main = main1)
   unlink(main1, recursive = TRUE)
} # }
```
