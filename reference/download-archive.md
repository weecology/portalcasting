# Download the Portal Predictions Repository Archive

Downloads a specific `version` of the Portal Predictions repository from
either GitHub or Zenodo (based on `source`) into the `<main>/raw` sub.

## Usage

``` r
download_archive(
  main = ".",
  resources_sub = "resources",
  version = "latest",
  source = "github",
  quiet = FALSE,
  verbose = FALSE,
  force = FALSE,
  pause = 30,
  timeout = getOption("timeout")
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

  `character` version of the data to download. Default `"latest"`
  downloads the most recent (by date published). `NULL` means no
  download.

- source:

  `character` indicator of the source for the download. Either
  `"github"` (default) or `"zenodo"`.

- quiet:

  `logical` indicator if progress messages should be quieted.

- verbose:

  `logical` indicator if detailed messages should be generated.

- force:

  `logical` indicator of whether or not existing files or folders (such
  as the archive) should be over-written if an up-to-date copy exists
  (most users should leave as `FALSE`).

- pause:

  Positive `integer` or integer `numeric` seconds for pausing during
  steps around unzipping that require time delay.

- timeout:

  Positive `integer` or integer `numeric` seconds for timeout on
  downloads. Temporarily overrides the `"timeout"` option in
  [`options`](https://rdrr.io/r/base/options.html).

## Value

`NULL`, [`invisible`](https://rdrr.io/r/base/invisible.html)-ly.

## Note

There are two calls to
[`base::Sys.sleep`](https://rdrr.io/r/base/Sys.sleep.html) for `pause`
seconds each to allow for the file unzipping, copying, and such to catch
up.

## See also

Other downloads:
[`download climate forecasts`](https://weecology.github.io/portalcasting/reference/download-climate-forecasts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "archive")
   create_dir(main = main1)
   download_archive(main = main1)
   unlink(main1, recursive = TRUE)
 } # }
```
