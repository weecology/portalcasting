options_all <- all_options(base = "~", main = "portalcasting_dev")
tree <- dirtree(base = "~", main = "portalcasting_dev")
read_all(tree)

new functions:
nbsGARCH (has arguments the same as nbGARCH)
foy (argument: dates, which has same validity as cast_dates)



steps in adding a model
1. write model function