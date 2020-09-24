# fix plot names when it has GROA plot ID

# Clean environment ####
rm(list = ls())

MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
HISTORY      <- read.csv("data/ForC_history.csv", stringsAsFactors = F)


# identify issues
old_plot_names <- HISTORY$plot.name



refor.type.to.plot.name.key <-  data.frame(plot.name = c("regrowth stand regenerating via spontaneous natural regeneration","regrowth stand regenerating via assisted natural regeneration","regrowth stand regenerating via initial tree planting", "diverse species plantation", "monoculture plantation", "intensive tree monocrop", "multistrata stand", "stand with tree intercropping", "silvopastoral system", "transitional ecosystem", "cropland", "pasture", "intact/ old growth stand", "burned", "harvest", "shifting cultivation"), row.names = c("SNR", "ANR", "ITP", "DP", "MP", "TMC", "MS", "TI", "SP", "TR", "C", "PA", "OG", "F", "H", "SC"), stringsAsFactors = F)

idx <- grep(paste(paste(refor.type.to.plot.name.key$plot.name, rep(c("\\d*$"), nrow(refor.type.to.plot.name.key))), collapse = "|"),HISTORY$plot.name)

keep_track <- data.frame(history.ID = HISTORY$history.ID[idx],
           sites.sitename = HISTORY$sites.sitename[idx],
           old_plot_name = HISTORY$plot.name[idx],
           short_plot_name = substr(HISTORY$plot.name[idx], (nchar(HISTORY$plot.name[idx])-20), nchar(HISTORY$plot.name[idx])),
           GROA_site.id = gsub("GROA site.id #|,", "", unlist(regmatches(HISTORY$hist.notes[idx], gregexpr("GROA site.id #\\d*,", HISTORY$hist.notes[idx])))),
           GROA_plot.id =gsub("#", "", unlist(regmatches(HISTORY$hist.notes[idx], gregexpr("#\\d*$", HISTORY$hist.notes[idx])))),
           new_plot.name = NA,
           stringsAsFactors = F
)


setdiff(keep_track$old_plot_name, MEASUREMENTS$plot.name)

# remove plot extension when only one plot per site
site.id_unique <- names(which(tapply(keep_track$history.ID, keep_track$GROA_site.id, length)==1     ))

new_idx <- keep_track$GROA_site.id %in% site.id_unique
keep_track$new_plot.name[new_idx] <- sapply(regmatches(keep_track$old_plot_name[new_idx], regexpr(" \\d*$", keep_track$old_plot_name[new_idx]), invert = T), "[[", 1)


# replace plot extension by GROA plot.id
new_idx <- !keep_track$GROA_site.id %in% site.id_unique
keep_track$new_plot.name[new_idx] <- sapply(regmatches(keep_track$old_plot_name[new_idx], regexpr(" \\d*$", keep_track$old_plot_name[new_idx]), invert = T), "[[", 1)
keep_track$new_plot.name[new_idx] <- paste0(keep_track$new_plot.name[new_idx], " (GROA plot.id ", keep_track$GROA_plot.id[new_idx], ")" )

# chack it looks good
keep_track$new_short_plot_name <- substr(keep_track$new_plot.name, nchar(keep_track$new_plot.name)-20, nchar(keep_track$new_plot.name))

View(keep_track)

# actually replace in HISTORY and MEASUREMENTS

## in HISTORY
idx <- HISTORY$history.ID %in%  keep_track$history.ID
HISTORY$plot.name[idx] <- keep_track$new_plot.name[match(HISTORY$history.ID[idx], keep_track$history.ID)]

## in MEASUREMENTS
idx <- paste(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name) %in%  paste(keep_track$sites.sitename, keep_track$old_plot_name)
MEASUREMENTS$plot.name[idx] <- keep_track$new_plot.name[match(paste(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name)[idx], paste(keep_track$sites.sitename, keep_track$old_plot_name))]

# save ####
write.csv(MEASUREMENTS, "data/ForC_measurements.csv", row.names = F)
write.csv(HISTORY, "data/ForC_history.csv", row.names = F)
write.csv(keep_track, "scripts/z_archive/to_delete_later.csv", row.names = F)
