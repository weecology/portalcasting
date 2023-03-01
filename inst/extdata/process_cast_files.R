main <- "~/pct"


lf <- list.files(file.path(main, "forecasts"))
cm <- read.csv(file.path(main, "forecasts", "casts_metadata.csv"))



nci <- nrow(cm)
ci <- numeric(nci)
nsp <- numeric(nci)
spp <- NULL

for(i in 1:nci){

  ci[i] <- cm$cast_id[i]
  ct <- read.csv(file.path(main, "forecasts", paste0("cast_id_", ci[i], "_cast_tab.csv")))
  ct$species[is.na(ct$species)] <- "NA"
  sp <- unique(ct$species)
  spp <- c(spp, sp)
  nsp[i] <- length(sp)

}

ncsm <- sum(nsp)
cm2 <- data.frame(matrix(NA, nrow = ncsm, ncol = ncol(cm) + 1))
colnames(cm2) <- c(colnames(cm), "species")
s1 <- c(1, cumsum(nsp[-nci]))
s2 <- cumsum(nsp)

for(i in 1:nci){

  cm2[s1[i]:s2[i], 1:ncol(cm)] <- cm[i, ] 
  cm2[s1[i]:s2[i], ncol(cm) + 1] <- spp[s1[i]:s2[i]]

}

cm2$old_cast_id <- cm2$cast_id
cm2$new_cast_id <- paste0(cm2$cast_id, "_", cm2$species)
cm2$cast_id <- 1:ncsm
head(cm2)

write.csv(cm2, file.path(main, "forecasts", "casts_metadata2.csv"))
write.table(cm2, file.path(main, "forecasts", "casts_metadata2.txt"), sep = "\t")


head(cm2)

write_yaml(cm2, file.path(main, "forecasts", "casts_metadata2.yaml"), column.major = FALSE)






