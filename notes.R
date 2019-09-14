devtools::load_all()
main <- "~/hindcasting"


select_casts(main)

portalcast(main = main,models = c("AutoArima", "ESSS", "NaiveArima"), 
           end_moons = 512)