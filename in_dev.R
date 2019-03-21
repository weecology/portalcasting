options_all <- all_options(base = "~", main = "portalcasting_dev070")
setup_dir(options_all)
portalcast(options_all)

tree <- dirtree(base = "~", main = "portalcasting_dev070")
read_all(tree)

new functions:
nbsGARCH (has arguments the same as nbGARCH)
foy (argument: dates, which has same validity as cast_dates)


tree <- dirtree("~", "portalcasting_hindev", subdirs());
f_a <- pevGARCH(tree, level = "All", lag = 6, quiet = FALSE);

f_c <- nbsGARCH(tree, level = "Controls", quiet = FALSE);

plot_err_lead_spp_mods(tree)
al<-read_all(dirtree(base="~", main="portalcasting_hindev"))


tree <- dirtree(base = "~", main = "portalcasting_hindev")
read_casts(tree, cast_type = "hindcasts")

plot_cov_RMSE_mod_spp(
dirtree(base = "~", main = "portalcasting_dev"))