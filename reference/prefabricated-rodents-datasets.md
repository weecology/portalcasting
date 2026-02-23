# Provide the Names or Controls for the Prefabricated Rodent Datasets

Create a `character` vector of the names of the pre-fabricated (prefab)
rodent datasets or species or a `list` of their controls

## Usage

``` r
prefab_datasets()

prefab_datasets_controls()

prefab_species(main = ".")
```

## Arguments

- main:

  `character` value of the name of the main component of the directory
  tree.

## Value

`prefab_datasets`: `character` vector of dataset names.  
`prefab_datasets_controls`: `list` of dataset controls.  
`prefab_species`: `character` vector of species abbreviations.

## Examples

``` r
  prefab_datasets_controls( )
  prefab_datasets( )
```
