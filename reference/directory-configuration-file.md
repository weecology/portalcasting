# Create, Update, and Read the Directory Configuration File

The directory configuration file is a special file within the directory
setup and has its own set of functions.  
  
`write_directory_configuration` creates the YAML metadata configuration
file. It is (and should only be) called from within
[`setup_dir`](https://weecology.github.io/portalcasting/reference/directory-creation.md),
as it captures information about the compute environment used to
instantiate the directory.  
  
`read_directory_configuration` reads the YAML config file into the R
session.  
  
`read_directory_settings` reads the YAML config file into the R session
and pulls just the directory settings list in.

## Usage

``` r
write_directory_configuration(
  main = ".",
  settings = directory_settings(),
  quiet = FALSE,
  verbose = FALSE
)

read_directory_configuration(main = ".")

read_directory_settings(main = ".")

update_directory_configuration(main = ".")
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- settings:

  `list` of controls for the directory, with defaults set in
  [`directory_settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md).

- quiet:

  `logical` indicator if progress messages should be quieted.

- verbose:

  `logical` indicator of whether or not to print out all of the
  messages.

## Value

`list` of directory configurations,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.

## See also

Directory orchestration functions:
[`directory creation`](https://weecology.github.io/portalcasting/reference/directory-creation.md),
[`directory filling`](https://weecology.github.io/portalcasting/reference/directory-filling.md),
[`directory paths`](https://weecology.github.io/portalcasting/reference/directory-paths.md),
[`directory settings`](https://weecology.github.io/portalcasting/reference/directory-settings.md)

File read-write functions:
[`read write data`](https://weecology.github.io/portalcasting/reference/read-write-data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "standard")
   setup_dir(main = main1)

   settings1 <- read_directory_settings(main = main1)
   config1   <- read_directory_configuration(main = main1)

   unlink(main1, recursive = TRUE)
} # }
```
