# Build and Launch the Portal Forecast Web Application

`run_app` constructs and launches a local version of the web application
by running [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html)
pointed to the `app` subdirectory in the local `portalcasting` package
folder.  
  
`global_list` creates a list of values that are globally available in an
app run.

## Usage

``` r
run_app(main = ".")

global_list(main = ".")
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

## Value

`global_list`: a `list` of values that will be globally available in the
app.  
`run_app` runs the app itself.

## See also

Other shinyapp:
[`portalcasting app selection helpers`](https://weecology.github.io/portalcasting/reference/portalcasting-app-selection-helpers.md),
[`portalcasting app server`](https://weecology.github.io/portalcasting/reference/portalcasting-app-server.md),
[`portalcasting app ui`](https://weecology.github.io/portalcasting/reference/portalcasting-app-ui.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "app")
   setup_dir(main = main1)

   global_list(main = main1)

   if (getShinyOption("shiny.launch.browser", FALSE)) {
     run_app(main = main1)
   }

   unlink(main1, recursive = TRUE)
 } # }
```
