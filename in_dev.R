devtools::load_all()
options_all <- all_options(base = "~", main = "portalcasting_dev1",
                          download_existing_predictions = FALSE)
#setup_dir(options_all)
tree <- dirtree(base = "~", main = "portalcasting_dev")
portalcast(options_all)

# currently running the models today to see how the figures will look on friday


all <- read_data(tree, "all")


plot(all$newmoonnumber, all$total, type = "l")

y <- all$total
x <- all$newmoonnumber

abundances <- all
level <- "All"
metadata <- read_data(tree, "metadata")

mo <- LTAvg(all, metadata, level)

