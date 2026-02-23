# Generate the User Interface for the Web App

`app_ui` constructs the user interface (UI) for the web application by
updating the static pages (models and rodent profiles) then running
[`fluidPage`](https://rdrr.io/pkg/shiny/man/fluidPage.html) on the UI
components.  
`write_rodent_profiles_tab_html` and `write_model_tab_html` build and
write-out static html files for the rodent profiles and models tabs
during
[`fill_app`](https://weecology.github.io/portalcasting/reference/directory-filling.md).  
The `<_>_href` functions provide simplified calls to hyperlinked texts
that are repeatedly used.  
See `Details` for hierarchy of functions.

## Usage

``` r
app_ui(global = global_list())

app_theme()

main_panel(global = global_list())

title_panel()

subtitle_panel()

about_tab(global = global_list())

models_tab(global = global_list())

rodents_profiles_tab(global = global_list())

forecast_tab(global = global_list())

forecast_tab_input_selection_checks_row()

forecast_tab_input_selection_row(global = global_list())

forecast_tab_input_selection_row_species(global = global_list())

forecast_tab_input_selection_row_dataset(global = global_list())

forecast_tab_input_selection_row_model(global = global_list())

forecast_tab_input_selection_row_historic_end_newmoonnumber(
  global = global_list()
)

evaluation_tab(global = global_list())

evaluation_tab_input_selection_checks_row()

evaluation_tab_input_selection_row(global = global_list())

evaluation_tab_input_selection_row_species(global = global_list())

evaluation_tab_input_selection_row_dataset(global = global_list())

evaluation_tab_input_selection_row_model(global = global_list())

evaluation_tab_input_selection_row_historic_end_newmoonnumber(
  global = global_list()
)

evaluation_tab_input_selection_row_newmoonnumber(global = global_list())

covariates_tab(global = global_list())

data_sources_section()

portal_project_href(text = "The Portal Project")

portal_data_href(text = "The Portal Data Repository")

portal_weather_href(text = "weather data")

portal_ndvi_href(text = "NDVI data")

write_rodents_profiles_tab_html(main = ".")

write_models_tab_html(main = ".")
```

## Arguments

- global:

  A `list` of global values for the app.

- text:

  `character` value of the text used in
  [`htmltools::a`](https://rstudio.github.io/htmltools/reference/builder.html).

- main:

  `character` value of the name of the main component of the directory
  tree.

## Value

A UI definition, component shiny tags, or bootswatch theme.

## Details

The UI is hierarchical built as:

- `app_ui`

  - `title_panel`

  - `subtitle_panel`

  - `main_panel`

    - `forecast_tab`

      - `forecast_tab_input_selection_row`

        - `forecast_tab_input_selection_row_species`

        - `forecast_tab_input_selection_row_dataset`

        - `forecast_tab_input_selection_row_model`

        - `forecast_tab_input_selection_row_historic_end_newmoonnumber`

      - `forecast_tab_input_selection_checks_row` \# commented out, but
        available for checking reactive inputs in dev

      - `plot_forecast_ts`

      - `plot_forecast_point`

    - `evaluation_tab`

      - `evaluation_tab_input_selection_row`

        - `evaluation_tab_input_selection_row_species`

        - `evaluation_tab_input_selection_row_dataset`

        - `evaluation_tab_input_selection_row_model`

        - `evaluation_tab_input_selection_row_historic_end_newmoonnumber`

        - `evaluation_tab_input_selection_row_newmoonnumber`

      - `evaluation_tab_input_selection_checks_row` \# commented out,
        but available for checking reactive inputs in dev

      - `plot_forecast_point`

      - `plot_forecasts_cov_RMSE`

    - `about_tab`

      - [`htmltools::includeMarkdown`](https://rstudio.github.io/htmltools/reference/include.html)

    - `models_tab`

      - [`htmltools::includeHTML`](https://rstudio.github.io/htmltools/reference/include.html)

    - `rodents_profiles_tab`

      - [`htmltools::includeHTML`](https://rstudio.github.io/htmltools/reference/include.html)

    - `covariates_tab`

      - `data_sources_section`

## See also

Other shinyapp:
[`portalcasting app`](https://weecology.github.io/portalcasting/reference/portalcasting-app.md),
[`portalcasting app selection helpers`](https://weecology.github.io/portalcasting/reference/portalcasting-app-selection-helpers.md),
[`portalcasting app server`](https://weecology.github.io/portalcasting/reference/portalcasting-app-server.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "app_ui")
   setup_dir(main = main1)

   global <- global_list(main = main1)
 
   app_ui(global = global)
   title_panel( )
   subtitle_panel( )
   main_panel(global = global)
   forecast_tab(global = global)
   forecast_tab_input_selection_row(global = global)
   forecast_tab_input_selection_row_species(global = global)
   forecast_tab_input_selection_row_dataset(global = global)
   forecast_tab_input_selection_row_model(global = global)
   forecast_tab_input_selection_row_historic_end_newmoonnumber(global = global)
   forecast_tab_input_selection_checks_row( )
   evaluation_tab(global = global)
   evaluation_tab_input_selection_row(global = global)
   evaluation_tab_input_selection_row_species(global = global)
   evaluation_tab_input_selection_row_dataset(global = global)
   evaluation_tab_input_selection_row_model(global = global)
   evaluation_tab_input_selection_row_historic_end_newmoonnumber(global = global)
   evaluation_tab_input_selection_row_newmoonnumber(global = global)
   evaluation_tab_input_selection_checks_row( )
   about_tab( )
   models_tab(global = global)
   rodents_profiles_tab(global = global)
   covariates_tab(global = global)
   data_sources_section( )

   unlink(main1, recursive = TRUE)
 } # }
```
