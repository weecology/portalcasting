devtools::document()
devtools::load_all()
main <- "~/hindcasting"


setup_dir(main)

portalcast(main = main,models = c("AutoArima", "ESSS", "NaiveArima"), 
           end_moons = 515:518)


plot_cast_ts(main=main)
plot_cast_point(main=main)
plot_cast_point(main=main,with_census=T)
plot_casts_err_lead(main)
