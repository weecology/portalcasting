# Facilitate Adding Models to a Directory

Create a new model's controls. Using the `model_controls_template` file
as a basis for the `list` and leveraging element-specific functions to
fill in details.  
Each of the specific `new_model_< >` functions wraps an
[`update_list`](https://weecology.github.io/portalcasting/reference/update_list.md)
call starting with the `model_controls_template` as the main list and
taking any named elements as inputs via `...`.

## Usage

``` r
model_controls_template()

add_new_model(main = ".", new_model_controls = model_controls_template())

new_model_controls(...)

new_model_metadata(...)

new_model_fit(...)

new_model_forecast(...)

new_model_interpolate(...)

new_model_datasets(...)

new_model_response(...)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- new_model_controls:

  `list` of controls for any new models (not in the prefab models)
  listed in `models` that are to be added to the control list and file.

- ...:

  Named `list` of arguments passed to
  [`update_list`](https://weecology.github.io/portalcasting/reference/update_list.md).

## Value

`model_controls_template`: `list` of named model controls elements, many
as `NULL`.  
`new_model_controls`: `list` of named model controls.  
`new_model_metadata`: `list` of named model metadata elements to the
controls `list`.  
`new_model_fit`: `list` of named model fit function and argument
elements to the controls `list`.  
`new_model_forecast`: `list` of named forecast function and argument
elements to the controls `list`.  
`new_model_interpolate`: `list` of named interpolation requirements
elements to the controls `list`.  
`new_model_datasets`: `list` of named dataset elements to the controls
`list`.  
`new_model_response`: `list` of named response data description elements
to the controls `list`.  
`add_new_model`: model controls `list` for the new model,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.

## Details

Having been created using `new_model_controls`, the new model's controls
can either be added to the directory at directory creation (via
[`setup_dir`](https://weecology.github.io/portalcasting/reference/directory-creation.md)
or related `setup_<>` functions) or update (via
[`update_dir`](https://weecology.github.io/portalcasting/reference/directory-creation.md))
steps or via `add_new_model`.

## See also

Directory customization functions:
[`new datasets`](https://weecology.github.io/portalcasting/reference/new-datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "new_model_controls")
   setup_dir(main = main1)

   model_controls_template( )

   new_controls <- new_model_controls(metadata = new_model_metadata(name       = "newmod", 
                                                                    print_name = "New Model"),
                                      fit      = new_model_fit(fun  = "arima", 
                                                               args = list(x = "abundance")),
                                      response = new_model_response(link           = "normal", 
                                                                    type           = "distribution", 
                                                                    scoring_family = "normal"))
   added <- add_new_model(main               = main1, 
                          new_model_controls = new_controls)

   portalcast(main     = main1, 
              models   = "newmod", 
              datasets = "all", 
              species  = c("DM", "PP", "total"))


   unlink(main1, recursive = TRUE)
 } # }
```
