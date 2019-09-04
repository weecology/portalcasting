# to do
# update vignettes
# evaluation (do in next iteration?)

remove "script" from model script controls
change data_set to name in rodent controls
standardize control, controls, etc

why still getting no controls for newmon

ndc <- rodents_control("newdata", level = "Treatment", treatment = "removal",
                arg_checks = FALSE)

prep_rodents(main = main, data_sets = "newdata", controls_rodents = ndc,
                arg_checks = FALSE)


 model_script_control(name = "newmod", 
                                  data_sets = "newdata",
                                  covariatesTF = TRUE, lag = 0,
                                  arg_checks = TRUE)
devtools::document()

main <- "~/test"
setup_dir(main)

portalcast(main)

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










