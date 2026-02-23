# Find an Object's Host Package and Version Information

Locate basic package information of an R object. If nothing is input, it
operates on itself.  
If the object is sourced through multiple packages, each package and its
version are included.

## Usage

``` r
package_version_finder(what)
```

## Arguments

- what:

  An R object.

## Value

`list` of the object, its class, the packages it is sourced from /
through, and the versions of those packages.

## See also

Other utilities:
[`file_ext()`](https://weecology.github.io/portalcasting/reference/file_ext.md),
[`foy()`](https://weecology.github.io/portalcasting/reference/foy.md),
[`ifnull()`](https://weecology.github.io/portalcasting/reference/ifnull.md),
[`messages`](https://weecology.github.io/portalcasting/reference/messages.md),
[`named_null_list()`](https://weecology.github.io/portalcasting/reference/named_null_list.md),
[`round_na.interp()`](https://weecology.github.io/portalcasting/reference/round_na.interp.md),
[`update_list()`](https://weecology.github.io/portalcasting/reference/update_list.md)

## Examples

``` r
   package_version_finder( )
```
