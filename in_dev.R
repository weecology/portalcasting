options_all <- all_options(base = "~", main = "portalcasting_dev")
tree <- dirtree(base = "~", main = "portalcasting_dev")
read_all(tree)

new functions:
nbsGARCH (has arguments the same as nbGARCH)
foy (argument: dates, which has same validity as cast_dates)


tree <- dirtree("~", "portalcasting_dev", subdirs());
f_a <- nbsGARCH(tree, level = "All", quiet = FALSE);
f_c <- nbsGARCH(tree, level = "Controls", quiet = FALSE);

plot_err_lead_spp_mods(tree)


#' 
#' @examples
#' \dontrun{
#' 
#' setup_dir()
#' nbsGARCH()
#' }
#' 