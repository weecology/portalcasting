
devtools::document()
main <- "~/sandbox"
setup_sandbox(main = main)

?add_new_model

model_controls_template()

new_model_controls()

rm(list=ls())




new_controls <- new_model_controls(metadata = new_model_metadata(name = "newmod", print_name = "New Model"),
                                   fit      = new_model_fit(fun = "arima", args = list(x = "abundance")),
                                   response = new_model_response(link = "normal", type = "distribution", scoring_family = "normal"))


added <- add_new_model(main = main, new_model_controls = new_controls)


names(read_model_controls(main = main))


portalcast(main = main, models = "newmod", datasets = "all", species = c("DM", "PP", "total"))





main2 <- "~/sandbox2"


new_controls <- new_model_controls(metadata = new_model_metadata(name = "newmod", print_name = "New Model"),
                                   fit      = new_model_fit(fun = "arima", args = list(x = "abundance")),
                                   response = new_model_response(link = "normal", type = "distribution", scoring_family = "normal"))





setup_sandbox(main = main2, new_model_controls = list(newmod = new_controls), models = c(prefab_models(), "newmod"))

portalcast(main = main2, models = "newmod", datasets = "all", species = c("DM", "PP", "total"))



main3 <- "~/sandbox3"
new_controls <- new_dataset_controls(metadata = new_dataset_metadata(name = "newdata"),
                                     args     = new_dataset_args(name = "newdata", filename = "rodents_newdata.csv"))

add_new_dataset(main = main, new_dataset_controls =  new_controls)

prepare_rodents(main = main, new_dataset_controls = list(newdata = new_controls), datasets = c(prefab_datasets(), "newdata"))

setup_sandbox(main = main3, new_dataset_controls = list(newdata = new_controls), datasets = c(prefab_datasets(), "newdata"))


