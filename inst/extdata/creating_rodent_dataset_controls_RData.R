############################################################################################################################
# 
#  Purpose: To edit the rodent_dataset_control internal data file available within the package
#  
#  The controls for the rodents datasets are organized according to lists with three elements: 
#
#    metadata	name, description, classifications, etc
#    fun		the function used to create the dataset
#    args		the arguments used in fun 
#
#  Each dataset's list is an entry within the overall list contained within the rodent_dataset_controls.yaml file
#
#  New datasets can be made available within the package by adding additional entries to the yaml file 
# 
        

# dependencies: yaml


rodent_dataset_controls <- yaml::read_yaml("./inst/extdata/rodent_dataset_controls.yaml")

save(rodent_dataset_controls = rodent_dataset_controls, file = "data/rodent_dataset_controls.RData")
