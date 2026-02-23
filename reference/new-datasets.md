# Facilitate Adding Datasets to a Directory

Create a new dataset's controls. Using the `dataset_controls_template`
file as a basis for the `list` and leveraging element-specific functions
to fill in details.  
Each of the specific `new_dataset_< >` functions wraps an
[`update_list`](https://weecology.github.io/portalcasting/reference/update_list.md)
call starting with the `dataset_controls_template` as the main list and
taking any named elements as inputs via `...`.

## Usage

``` r
dataset_controls_template()

add_new_dataset(
  main = ".",
  new_dataset_controls = dataset_controls_template(),
  models = NULL
)

new_dataset_controls(...)

new_dataset_metadata(...)

new_dataset_fun(fun = dataset_controls_template()$fun)

new_dataset_args(...)
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

- new_dataset_controls:

  `list` of controls for any new datasets (not in the prefab datasets)
  listed in `datasets` that are to be added to the control list and
  file.

- models:

  `character` vector of the names of the models that are to have their
  controls updated to include the new dataset.

- ...:

  Named `list` of arguments passed to
  [`update_list`](https://weecology.github.io/portalcasting/reference/update_list.md).

- fun:

  `character` value of the generation function.

## Value

`dataset_controls_template`: `list` of named dataset controls elements,
many as `NULL`.  
`new_dataset_controls`: `list` of named dataset controls.  
`new_dataset_metadata`: `list` of named dataset metadata elements to the
controls `list`.  
`new_dataset_fun`: `character` of dataset generation function.  
`new_dataset_args`: named `list` of argument elements to the generating
function.  
`add_new_dataset`: dataset controls `list` for the new dataset,
[`invisible`](https://rdrr.io/r/base/invisible.html)-ly.

## Details

Having been created using `new_dataset_controls`, the new dataset's
controls can either be added to the directory at directory creation (via
[`setup_dir`](https://weecology.github.io/portalcasting/reference/directory-creation.md)
or related
[`setup_<>`](https://weecology.github.io/portalcasting/reference/directory-creation.md)
functions) or update (via
[`update_dir`](https://weecology.github.io/portalcasting/reference/directory-creation.md))
steps or via `add_new_dataset`.

## See also

Directory customization functions:
[`new models`](https://weecology.github.io/portalcasting/reference/new-models.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   main1 <- file.path(tempdir(), "new_dataset_controls")
   setup_dir(main = main1)

   dataset_controls_template( )

   args <- new_dataset_args(name     = "newdata", 
                            filename = "rodents_newdata.csv")

   new_controls <- new_dataset_controls(metadata = new_dataset_metadata(name = "newdata"),
                                        args     = args)

   added <- add_new_dataset(main                 = main1, 
                            new_dataset_controls = new_controls,
                            models               = "AutoArima")
 
   portalcast(main     = main1, 
              datasets = "newdata", 
              models   = "AutoArima")

   unlink(main1, recursive = TRUE)
 } # }
```
