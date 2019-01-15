#find and convert special characters in ForC

##these could be in sites.sitename, plot.name, citation.ID, citation.title, citation.author

##NOTE: the companion to this code is the .txt file named "special_characters_R.txt." Special characters are located here due to encoding issues with R. The .txt file is saved with Unicode encoding and should only ever be saved as such.

setwd("E:/Github_SCBI/ForC_private/data to add/srdb/new_data")
meas <- read.csv("E:/Github_SCBI/ForC_private/data to add/srdb/new_data/new_MEASUREMENTS.csv")

#list of accented characters (from https://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding)
specials <- c()

#####################################################################################
## **** INSERT "SPECIALS" VECTOR HERE FROM "special_characters_R.txt" ****
#####################################################################################


#1 find out occurrences of special characters in the columns ####

##"symbol" has the special characters found within SRDB at one point of checking. It is assumed more can be found, at which point the "symbol" and "equiv" vectors in the txt file "special_characters_R.txt" should be updated.

symbol <- c()
equiv <- c()
#####################################################################################
## **** INSERT "SYMBOL" AND "EQUIV" VECTORS HERE FROM "special_characters_R.txt" ****
#####################################################################################

cols <- c('citation.ID', 'plot.name', 'sites.sitename')
proto <- meas #so as to preserve meas as the original. After the characters are determined, meas is fixed in #1b.

#1a combined for-loop ####
for (h in seq(along=cols)) {
  colname <- cols[h]
  
  proto[,colname] <- gsub("NAC", NA, proto[,colname])
  simple <- proto[!is.na(proto[,colname]), ] #now this contains only the citation.ID that are not NA
  
  ##these are the strings containing special characters
  matches <- unique (grep(paste(specials,collapse="|"), 
                          simple[,colname], value=TRUE))
  
  ##the below code will correctly substitute the equivalent characters.
  for (i in seq(along=symbol)){
    for (j in seq(along=equiv)){
      if(i==j){
        sym <- symbol[i]
        eq <- equiv[j]
        proto[,colname] <- gsub(sym, eq, proto[,colname])
      }
    }
  }
  
  ##double-check: this should be an empty value now
  matches <- unique (grep(paste(specials,collapse="|"), proto[,colname], value=TRUE))
}

##this is to triple check no special characters are left in each column
matches_citation <- unique (grep(paste(specials,collapse="|"), proto$citation.ID, value=TRUE))
matches_plot <- unique (grep(paste(specials,collapse="|"), proto$plot.name, value=TRUE))
matches_sites <- unique (grep(paste(specials,collapse="|"), proto$sites.sitename, value=TRUE))

#1b Now run the for-loop for the original file and save. ####
for (h in seq(along=cols)) {
  colname <- cols[h]
  
  ##the below code will correctly substitute the equivalent characters.
  for (i in seq(along=symbol)){
    for (j in seq(along=equiv)){
      if(i==j){
        sym <- symbol[i]
        eq <- equiv[j]
        meas[,colname] <- gsub(sym, eq, meas[,colname])
      }
    }
  }
}

write.csv(meas, "new_MEASUREMENTS.csv", row.names=FALSE)

#1c double-checking with other punctuation ####
##Using 'punct' should only be for double-checking, as often the characters in "specials" above and "symbols" below will be accompanied by extraneous punctuation anyways.
##when checking for punct, the "grep" function in the for-loop doesn't work. 
##As in, doing "punct %in% ForC_cite$citation.title" will return with all FALSE, whereas doing the full grep function below will return every single value.

punct <- c('\\!', '\\#', '\\$', '\\%', '\\&', '\\*', '\\+', '\\/', '\\<', '\\=', '\\>', '\\?', '\\@', '\\[',  '\\]', '\\^', '\\`', '\\{', '\\|', '\\}', '\\~')


#2 below is the separate work-through for each column. The for-loop in #1a massively simplifies this ####
#2a citation.ID ####
proto <- meas #so as to leave meas as normal. Only edit for gsubbing the mistakes.
proto$citation.ID <- gsub("NAC", NA, proto$citation.ID)
simple <- proto[!is.na(proto$citation.ID),] #now this contains only the citation.ID that are not NA

##these are the strings containing special characters
matches_citation <- unique (grep(paste(specials,collapse="|"), 
                                 simple$citation.ID, value=TRUE))

##subset the simple df and run the loop to make sure substitution works. Then apply to meas.
simple <- simple[simple$citation.ID %in% matches_citation, ]

##the below code will correctly substitute the equivalent characters.


for (i in seq(along=symbol)){
  for (j in seq(along=equiv)){
    if(i==j){
      sym <- symbol[i]
      eq <- equiv[j]
      #simple$citation.ID <- gsub(sym, eq, simple$citation.ID)
      meas$citation.ID <- gsub(sym, eq, meas$citation.ID)
    }
  }
}

##double-check: this should be an empty value now
matches_citation <- unique (grep(paste(specials,collapse="|"), meas$citation.ID, value=TRUE))

#2b plot.name ####
proto <- meas #so as to leave meas as normal. Only edit for gsubbing the mistakes.
proto$plot.name <- gsub("NAC", NA, proto$plot.name)
simple <- proto[!is.na(proto$plot.name), ]

matches_plots <- unique (grep(paste(specials,collapse="|"), 
                              simple$plot.name, value=TRUE))

##subset the simple df and run the loop to make sure substitution works. Then apply to meas.
simple <- simple[simple$citation.ID %in% matches_citation, ]

##the below code will correctly substitute the equivalent characters.


for (i in seq(along=symbol)){
  for (j in seq(along=equiv)){
    if(i==j){
      sym <- symbol[i]
      eq <- equiv[j]
      #simple$citation.ID <- gsub(sym, eq, simple$citation.ID)
      meas$citation.ID <- gsub(sym, eq, meas$citation.ID)
    }
  }
}

##double-check: this should be an empty value now
matches_citation <- unique (grep(paste(specials,collapse="|"), meas$citation.ID, value=TRUE))

#2c sites.sitename ####
proto <- meas #so as to leave meas as normal. Only edit for gsubbing the mistakes.
proto$sites.sitename <- gsub("NAC", NA, proto$sites.sitename)
simple <- proto[!is.na(proto$sites.sitename),]

matches_sites <- unique (grep(paste(specials,collapse="|"), 
                              simple$sites.sitename, value=TRUE))

simple <- simple[simple$sites.sitename %in% matches_sites, ]

for (i in seq(along=symbol)){
  for (j in seq(along=equiv)){
    if(i==j){
      sym <- symbol[i]
      eq <- equiv[j]
      #if (sym %in% result$sites.sitename)
      simple$sites.sitename <- gsub(sym, eq, simple$sites.sitename)
    }
  }
}
