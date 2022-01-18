############################################################################################################################
# 
#  Purpose: To edit the model_controls internal data file available within the package
#  
#  The controls for the models are organized according to lists with four elements: 
#
#    metadata	name, description, classifications, etc
#    fun		the function used to implement the model
#    args		named arguments used in fun 
#    datasets     names of datasets to apply the model to
#
#  Each model's list is an entry within the overall list contained within the model_controls.yaml file
#
#  New models can be made available within the package by adding additional entries to the yaml file 
# 
# dependencies: yaml


model_controls <- yaml::read_yaml("./inst/extdata/model_controls.yaml")

if (!dir.exists("data")) {
  dir.create("data")
}
save(model_controls = model_controls, file = "data/model_controls.RData")


# Currently not included
#
#    simplexEDM = list(name = "simplexEDM", 
#                      data_sets = c("all_interp", "controls_interp", 
#                                    "exclosures_interp", 
#                                     "dm_controls_interp"), 
#                      covariatesTF = FALSE, lag = NA, max_E = 7),
#    GPEDM = list(name = "GPEDM", 
#                 data_sets = c("all_interp", "controls_interp", 
#                               "exclosures_interp", "dm_controls_interp"), 
#                 covariatesTF = FALSE, lag = NA, max_E = 7),