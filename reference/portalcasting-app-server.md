# Generate the Server Code for the Web App

Generate the Server Code for the Web App

## Usage

``` r
app_events()

app_server(input, output, session)

initial_reactive_values(global = global_list())

initial_output(main = ".", global, rv, output)

event_reaction(main, global, event, rv, input, output, session)

update_reactive_values(event, rv, input)

update_output(main, global, event, rv, input, output)

update_input(global, event, rv, input, session)
```

## Arguments

- input:

  `input` `list` for the UI.

- output:

  `output` `list` for the UI.

- session:

  Environment for the UI.

- global:

  A `list` of global values for the app.

- main:

  `character` value of the name of the main component of the directory
  tree.

- rv:

  [`reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  `list` for the UI.

- event:

  `character` value of the server event.  
  Options are managed by `app_events` – currently including
  `"forecast_tab_species"`, `"forecast_tab_dataset"`,
  `"forecast_tab_model"`, `"forecast_tab_historic_end_newmoonnumber"`,
  `"evaluation_tab_species"`, `"evaluation_tab_dataset"`,
  `"evaluation_tab_model"`, and
  `"evaluation_tab_historic_end_newmoonnumber"`)

## Value

`app_server`: an observer reference class object (see
[`observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html) and
[`observe`](https://rdrr.io/pkg/shiny/man/observe.html)).  
`initial_reactive_values`: a
[`reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
`list`.  
`initial_output`: an `output` `list`.  
`event_reaction`: updates the `rv`, `output`, and `input` `list`s, but
does not return them, per se.  
`update_reactive_values`: a
[`reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
`list`.  
`update_output`: an `output` `list`.  
`update_input`: updates the `input` `list`, but does not return it.  
`app_events`: `character` vector of event names.

## See also

Other shinyapp:
[`portalcasting app`](https://weecology.github.io/portalcasting/reference/portalcasting-app.md),
[`portalcasting app selection helpers`](https://weecology.github.io/portalcasting/reference/portalcasting-app-selection-helpers.md),
[`portalcasting app ui`](https://weecology.github.io/portalcasting/reference/portalcasting-app-ui.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "app")
   setup_dir(main = main1)

   global <- global_list(main = main1)
 
   rv     <- initial_reavtive_values(global = global)

   output <- initial_output(main   = main1,
                            global = global,
                            rv     = rv,
                            output = list())

   unlink(main1, recursive = TRUE)
 } # }
```
