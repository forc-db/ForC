rm(list = ls())


MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
HISTORY <- read.csv("data/ForC_history.csv", stringsAsFactors = F)


plotnames_that_have_to_change <- read.csv("C:/Users/HerrmannV/Dropbox (Smithsonian)/GitHub/forc-db/ForC_private/data to add/srdb/new_data/plot_names_changes_for_issue_183.csv")


sub <- MEASUREMENTS[paste0(MEASUREMENTS$sites.sitename, MEASUREMENTS$date) %in% paste0(plotnames_that_have_to_change$sites.sitename, plotnames_that_have_to_change$date) & MEASUREMENTS$ForC.investigator %in% "Ben Bond-Lamberty",  ]

sub$establishement_date <- floor(as.numeric(sub$date)) - as.numeric(sub$stand.age)


check <- unique(sub[, c("sites.sitename", "establishement_date")])
check[order(check$sites.sitename), ]



for(i in 153:nrow(plotnames_that_have_to_change)) { # 1:nrow(plotnames_that_have_to_change)
  cat(i)
  s_p <- paste0(plotnames_that_have_to_change$sites.sitename, plotnames_that_have_to_change$old.plot.name)[i]
  date <- plotnames_that_have_to_change$date[i]

  sub <- MEASUREMENTS[paste0(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name) %in% s_p & MEASUREMENTS$date %in% date & MEASUREMENTS$ForC.investigator %in% "Ben Bond-Lamberty",  ]

  date_to_replace <- unique(floor(as.numeric(sub$date)) - as.numeric(sub$stand.age))
  if(length(date_to_replace) > 1 ) stop()
  
  new_plot.name <- unique(gsub("[0-9]{4}", date_to_replace, sub$plot.name ))
  new_plot.name <- gsub("(?<=[0-9]{4}).5", "", new_plot.name, perl = T)
  
  print((new_plot.name == plotnames_that_have_to_change$new.plot.name[i]))
  if(!(new_plot.name == plotnames_that_have_to_change$new.plot.name[i])) stop("not right replacement name")
  
  MEASUREMENTS[paste0(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name) %in% s_p & MEASUREMENTS$date %in% date & MEASUREMENTS$ForC.investigator %in% "Ben Bond-Lamberty",  ]$plot.name <- new_plot.name
  if(any(paste0(HISTORY$sites.sitename, HISTORY$plot.name) %in% s_p)) HISTORY[paste0(HISTORY$sites.sitename, HISTORY$plot.name) %in% s_p,  ]$plot.name <- new_plot.name
}


# fix history by deleting unrelevant rows (duplicated)
for(i in 1:nrow(plotnames_that_have_to_change)) {
  print(i)
  s_p <- paste0(plotnames_that_have_to_change$sites.sitename, plotnames_that_have_to_change$new.plot.name)[i]
  
  sub <- HISTORY[paste0(HISTORY$sites.sitename, HISTORY$plot.name) %in% s_p,]
  
  idx_row_to_dump <- which(sub$hist.cat %in% "Establishment" & regmatches(sub$plot.name, gregexpr("[0-9]{4}",sub$plot.name )) != sub$date)
  floor_HISTORY_ID_to_dump <- floor(sub$history.ID[idx_row_to_dump])
  
  HISTORY <- HISTORY[!floor(HISTORY$history.ID) %in% floor_HISTORY_ID_to_dump, ]
}

write.csv(MEASUREMENTS, file = "data/ForC_measurements.csv", row.names = F)
write.csv(HISTORY , file = "data/ForC_history.csv", row.names = F)
