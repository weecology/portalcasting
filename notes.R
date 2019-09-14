devtools::load_all()
main <- "~/hindcasting"


setup_dir(main)

portalcast(main = main,models = c("AutoArima", "ESSS", "NaiveArima"), 
           end_moons = 515:518)

ensemble_casts(main)