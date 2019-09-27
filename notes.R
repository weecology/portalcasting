working on a few things:
the logistic build
the whole "updating models" thingy
messaging

devtools::document()
devtools::load_all()
#main <- "c:/users/dappe/dropbox/uf/logistic_build"
main <- "c:/users/uf/dropbox/uf/logistic_build"
#main <- "~/logistic_build"
setup_dir(main)


devtools::check()

portalcast(main = main, models = c("logistic"))



portalcast(main = main,models = c("nbsGARCH"), 
           end_moons = 512)
plot_cast_ts(main=main)


plot_cast_point(main=main)
plot_cast_point(main=main,with_census=T)
plot_casts_err_lead(main)
plot_casts_cov_RMSE(main)

devtools::test(filter="01")
devtools::test(filter="02")
devtools::test(filter="03")
devtools::test(filter="04")
devtools::test(filter="05")
devtools::test(filter="06")
devtools::test(filter="07")
devtools::test(filter="08")
devtools::test(filter="09")
devtools::test(filter="10")
devtools::test(filter="11")
devtools::test(filter="12")
devtools::test(filter="13")
devtools::test(filter="14")
devtools::test(filter="15")
devtools::test(filter="16")
devtools::test(filter="17")
devtools::test(filter="18")
devtools::test(filter="19")
devtools::test(filter="20")
