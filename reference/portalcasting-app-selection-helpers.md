# Selection Helper Functions for the Portalcasting App

Construct vectors of available choices and make selections, with respect
to species, dataset, model, and moons.

## Usage

``` r
available_newmoonnumbers(
  global = global_list(),
  event = "initial_evaluation_tab",
  rv = NULL
)

selected_newmoonnumber(
  global = global_list(),
  event = "initial_evaluation_tab",
  rv = NULL
)

available_historic_end_newmoonnumbers(
  global = global_list(),
  event = "initial_forecast_tab",
  rv = NULL
)

selected_historic_end_newmoonnumber(
  global = global_list(),
  event = "initial_forecast_tab",
  rv = NULL
)

available_species(
  global = global_list(),
  event = "initial_forecast_tab",
  rv = NULL
)

selected_species(
  global = global_list(),
  event = "initial_forecast_tab",
  rv = NULL
)

available_datasets(
  global = global_list(),
  event = "initial_forecast_tab",
  rv = NULL
)

selected_dataset(
  global = global_list(),
  event = "initial_forecast_tab",
  rv = NULL
)

available_models(
  global = global_list(),
  event = "initial_forecast_tab",
  rv = NULL
)

selected_model(
  global = global_list(),
  event = "initial_forecast_tab",
  rv = NULL
)
```

## Arguments

- global:

  A `list` of global values for the app.

- event:

  `character` value of the server event.  
  Options include `"initial_forecast_tab"`, `"initial_evaluation_tab"`,
  `"forecast_tab_species"`, `"forecast_tab_dataset"`,
  `"forecast_tab_model"`, `"forecast_tab_historic_end_newmoonnumber"`,
  `"evaluation_tab_species"`, `"evaluation_tab_dataset"`,
  `"evaluation_tab_model"`,
  `"evaluation_tab_historic_end_newmoonnumber"`, and
  `"evaluation_tab_newmoonnumber"`)

- rv:

  [`reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  `list` for the UI.

## Value

`character` or `integer`, depending upon the function.

## See also

Other shinyapp:
[`portalcasting app`](https://weecology.github.io/portalcasting/reference/portalcasting-app.md),
[`portalcasting app server`](https://weecology.github.io/portalcasting/reference/portalcasting-app-server.md),
[`portalcasting app ui`](https://weecology.github.io/portalcasting/reference/portalcasting-app-ui.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "app_helpers")
   setup_dir(main = main1)

   gl <- global_list(main = main1)

   ft_species                    <- gl$initial_forecast_tab_selected_species
   ft_dataset                    <- gl$initial_forecast_tab_selected_species
   ft_model                      <- gl$initial_forecast_tab_selected_species
   ft_historic_end_newmoonnumber <- gl$initial_forecast_tab_selected_historic_end_newmoonnumber
   et_species                    <- gl$initial_forecast_tab_selected_species
   et_dataset                    <- gl$initial_forecast_tab_selected_species
   et_model                      <- gl$initial_forecast_tab_selected_species
   et_historic_end_newmoonnumber <- gl$initial_forecast_tab_selected_historic_end_newmoonnumber
   et_newmoonnumber              <- gl$initial_forecast_tab_selected_newmoonnumber

   rv <- list(forecast_tab_species                      = ft_species,
              forecast_tab_dataset                      = ft_dataset,
              forecast_tab_model                        = ft_model,
              forecast_tab_historic_end_newmoonnumber   = ft_historic_end_newmoonnumber,
              evaluation_tab_species                    = et_dataset,
              evaluation_tab_dataset                    = et_species,
              evaluation_tab_model                      = et_model,
              evaluation_tab_historic_end_newmoonnumber = et_historic_end_newmoonnumber,
              evaluation_tab_newmoonnumber              = et_newmoonnumber)

   available_models(global = gl, 
                    event  = "initial_forecast_tab")
   selected_model(global   = gl, 
                  event    = "initial_forecast_tab", 
                  rv         = rv1)

   available_datasets(global = gl, 
                      event  = "initial_forecast_tab")
   selected_dataset(global   = gl, 
                    event    = "initial_forecast_tab", 
                    rv       = rv1)

   available_species(global  = gl, 
                     event   = "initial_forecast_tab")
   selected_species(global   = gl, 
                    event    = "initial_forecast_tab", 
                    rv       = rv1)

   available_historic_end_newmoonnumbers(global = gl, 
                                         event  = "initial_forecast_tab")
   selected_historic_end_newmoonnumber(global   = gl, 
                                       event    = "initial_forecast_tab", 
                                       rv       = rv1)

   available_newmoonnumbers(global = gl, 
                            event  = "initial_evaluation_tab")
   selected_newmoonnumber(global   = gl, 
                          event    = "initial_evaluation_tab", 
                          rv       = rv1)

   unlink(main1, recursive = TRUE)
 } # }
```
