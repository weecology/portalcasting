# to do
# update vignettes
# read through the code documentation again
#   donttest examples
#   argument order alignment
# additional test coverage/new functions getting tested



devtools::load_all()
main <- "~/checks"
setup_dir(main)
ESSS(main=main)
portalcast(main=main, models="ESSS")


portalcast(main, end_moons = 512:521)

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
devtools::test(filter="14")
devtools::test(filter="15")
devtools::test(filter="16")
devtools::test(filter="17")
devtools::test(filter="18")
devtools::test(filter="19")










