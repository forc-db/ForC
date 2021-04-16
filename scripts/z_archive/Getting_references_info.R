# Getting reference info ####

# clear environment ####
rm(list = ls())

# load library ####
library(httr)
library(textcat)
library(xml2)
library(rvest)
# library(rscopus)

# set API key ####
# Elsevier_API = "915120d996e269067bf48dc0f9400ea0" 
# options("elsevier_api_key" = Elsevier_API)

# load data ####
CITATIONS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_citations.csv", stringsAsFactors = F, encoding = "UTF-8")

 
na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999", "") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}




# get citation, abstract and URL ####



# get the URL ####

pb <- txtProgressBar(min = 0, max = nrow(CITATIONS), style = 3)

idx <-  which(!my_is.na(CITATIONS$citation.doi) & my_is.na( CITATIONS$citation.url))
# all_urls <- NULL


for(i in idx) {
  
  DOI = CITATIONS$citation.doi[i]
  
the_url <- tryCatch(HEAD(paste0(ifelse(grepl("http", DOI), "", "http://doi.org/"), DOI))$url, error = function(e) e) 

if(DOI == "10.1139/XO7-019") the_url = "https://cdnsciencepub.com/doi/10.1139/X07-019"
if(DOI == "10.1007/BF00045057") the_url = "https://link.springer.com/article/10.1007/BF00045057"

if("error" %in% class(the_url)){
  # all_urls <- c(all_urls, the_url$message)
  next 
  } else  {
  # all_urls <- c(all_urls, the_url)
    CITATIONS$citation.url[i] <- the_url
  }

# print progress bar ####
setTxtProgressBar(pb, i)

}


# get the citation ####
idx <-  which((!my_is.na(CITATIONS$citation.doi) | !my_is.na( CITATIONS$citation.url)) & my_is.na(CITATIONS$citation.citation))

for(i in idx) {
  
  DOI = CITATIONS$citation.doi[i]
  the_url <- CITATIONS$citation.url[i]

  temp <- tryCatch(GET(modify_url("https://doi.org/", path = DOI), 
              config = list(followlocation = TRUE), add_headers(Accept = "text/x-bibliography")), error = function(e) e) 
  
  if("error" %in% class(temp)) temp <- tryCatch(GET(the_url, 
                                            config = list(followlocation = TRUE), add_headers(Accept = "text/x-bibliography")), error = function(e) e) 
  
  if(!"error" %in% class(temp)) CITATIONS$citation.citation[i] <- content(temp, as = "text", encoding = "UTF-8")
  
  # print progress bar ####
  setTxtProgressBar(pb, i)
  
}


## clen up citation.citations ####
CITATIONS$citation.citation[grepl("DOCTYPE html", CITATIONS$citation.citation)] <- NA
CITATIONS$citation.citation[grepl('status":"error"', CITATIONS$citation.citation)] <- NA
CITATIONS$citation.citation <- gsub("–", "-", CITATIONS$citation.citation)
CITATIONS$citation.citation <- gsub("—", "-", CITATIONS$citation.citation)
CITATIONS$citation.citation <- gsub("\n$", "", CITATIONS$citation.citation)


# get the abstract ####
idx <-  which(!my_is.na(CITATIONS$citation.url) & my_is.na( CITATIONS$citation.abstract))


for(i in idx) {
  
  the_url <- CITATIONS$citation.url[i]
  
  # the_url <- tryCatch(HEAD(paste0(ifelse(grepl("http", DOI), "", "http://doi.org/"), DOI))$url, error = function(e) e) 
  # 
  # if(DOI == "10.1139/XO7-019") the_url = "https://cdnsciencepub.com/doi/10.1139/X07-019"
  # if(DOI == "10.1007/BF00045057") the_url = "https://link.springer.com/article/10.1007/BF00045057"

  

  # prepare nodes depending on source ####
  if(grepl("link.springer.com", the_url)) {
    node = 'div[id*="Abs1"]' #Abs1-section', #.Para'
    node_offset = 2
    # token_line <- 'class="Abstract'
    # token_line_offset = 0
    # token_prior <- '<p class=\"Para\">'
    # token_after <- "</p></section>"
  }
  if(grepl("onlinelibrary.wiley.com", the_url)) {
    node = 'div[class*="abstract-group"]'
    node_offset = 1
    # token_line <- '<div class="article-section__content en main">'
    # token_line_offset = 2
    # token_prior <- '<p>'
    # token_after <- "</p>"
  }
  if(grepl("linkinghub.elsevier.com", the_url)) {
    # token_line <- '>Abstract</h2>'
    # token_line_offset = 0
    # token_prior <- 'Abstract</h2>'
    # token_after <- "</p></div></div>"
    node = 'div[id*="aep-abstract-sec"]|div[class*="abstract"]'
    node_offset = 1
    # node = "#sp0005|#aep-abstract-sec-id10""aep-abstract-sec-id7""aep-abstract-sec-id7"
    the_url <- paste0(gsub("linkinghub.elsevier.com/retrieve/pii/", "www.sciencedirect.com/science/article/abs/pii/", the_url), "?via%3Dihub")
  }
  if(grepl("www.sciencedirect.com", the_url)) {
    node = 'div[id*="aep-abstract-sec"]'
    node_offset = 1
  }
  if(grepl("fluxnet.org", the_url)) next
  if(grepl("esajournals.onlinelibrary.wiley.com", the_url)) {
    node = 'div[class*="content en main"]'
    node_offset = 1
  }
  if(grepl("agupubs.onlinelibrary.wiley.com", the_url)) {
    node = 'div[class*="content en main"]'
    node_offset = 1
  }
  if(grepl("www.tandfonline.com", the_url)) {
    node = 'div[class*=" abstractInFull"]'
    node_offset = 1
  }
  if(grepl("cdnsciencepub.com", the_url)) {
    node = 'section[typeof*="Abstract"]'
    node_offset = 1 # 2 would be french abstract
  }
  if(grepl("academic.oup.com", the_url)) {
    node = 'p[class*="chapter-para"]'
    node_offset = 1 # 2 would be french abstract
  }
  if(grepl("bg.copernicus.org", the_url)) {
    # r <- tryCatch(read_html(the_url), error = function(e) e)
    # cast <- as.character(tryCatch(html_nodes(r, 'a[href*="https://bg.copernicus.org/preprints/"]'), error = function(e) e))
    # the_url <- gsub("\">", "", regmatches(cast, regexpr('https://bg.copernicus.org/preprints/.*">', cast)))
    node = 'div[class="abstract"]'  
    node_offset = 1 
  }
  if(grepl("journals.plos.org", the_url)) {
    node = 'div[class*="abstract-content"]'
    node_offset = 1
  }
  if(grepl("www.cambridge.org", the_url)) {
    node = 'div[class="abstract"]'
    node_offset = 1
    # token_line <- '<div class="abstract" data-abstract-type="normal"><p>'
    # token_line_offset = 0
    # token_prior <- '<div class="abstract" data-abstract-type="normal"><p>'
    # token_after <- "</p></div></div>"
  
  }
  if(grepl("www.cnki.com.cn", the_url)) {
    the_url <- gsub("https://doi.org/http:", "http:", the_url)
    node = 'div[style="text-align:left;word-break:break-all"]'
    node_offset = 1
  }
  if(grepl("cif-ifc.org", the_url)) {
    node = 'section[id*="abstract"]'
    node_offset = 1
  }
  if(grepl("bioone.org", the_url)) {
    node = 'p[id*="ID0EL"]'
    node_offset = 1
  }



  r <- tryCatch(read_html(the_url), error = function(e) e)
  cast <- tryCatch(html_nodes(r, node), error = function(e) e)
  # if("error" %in% class(cast) & !"error" %in% class(r)) stop()
  if("error" %in% c(class(r), class(cast))) next
  CITATIONS$citation.abstract[i] <-  gsub("\n|(  {1,})", " ", html_text(cast)[node_offset])
  
  # print progress bar ####
  setTxtProgressBar(pb, i)
  rm(r)
  rm(cast)
}

## clean up citation.abstract ####
CITATIONS$citation.abstract <- gsub("^ *Abstract *", "", CITATIONS$citation.abstract, ignore.case = T)
CITATIONS$citation.abstract <- gsub("^ *Summary *", "", CITATIONS$citation.abstract)
CITATIONS$citation.abstract <- gsub("^\t*", "", CITATIONS$citation.abstract)
CITATIONS$citation.abstract <- gsub(" {2,}", " ", CITATIONS$citation.abstract)
CITATIONS$citation.abstract <- gsub("^\\. ", "", CITATIONS$citation.abstract)
CITATIONS$citation.abstract <- gsub("^ *", "", CITATIONS$citation.abstract)
CITATIONS$citation.abstract <- gsub("^\\[\\d\\] *", "", CITATIONS$citation.abstract)
CITATIONS$citation.abstract <- gsub("–", "-", CITATIONS$citation.abstract)
CITATIONS$citation.abstract <- gsub("—", "-", CITATIONS$citation.abstract)
CITATIONS$citation.abstract <- gsub("\n$", "", CITATIONS$citation.abstract)


# close progress bar 

close(pb)

# see what happened ####

sum(is.na(CITATIONS$citation.abstract[idx]))
sum(!is.na(CITATIONS$citation.abstract[idx]))


# Get language ####
library(cld3)
languages_key <- jsonlite::fromJSON("https://raw.githubusercontent.com/unicode-cldr/cldr-localenames-modern/master/main/en/languages.json", flatten = T)
languages_key <- unlist(languages_key$main$en$localeDisplayNames$languages)


lang_title <- cld3::detect_language(CITATIONS$citation.title)
lang_abstract <-cld3::detect_language(CITATIONS$citation.abstract)

idx_same <- lang_title == lang_abstract
idx_diff <- lang_title != lang_abstract

sum(idx_same, na.rm = T)
sum(idx_diff, na.rm = T)

View(cbind(lang_title[idx_diff], lang_abstract[idx_diff], CITATIONS$citation.title[idx_diff],  CITATIONS$citation.abstract[idx_diff]))

View(cbind(lang_title[idx_same], lang_abstract[idx_same], CITATIONS$citation.title[idx_same],  CITATIONS$citation.abstract[idx_same]))

## fix language ####

### keep when they are the same
lang_final <- rep(NA, length(lang_title))
lang_final[idx_same & !is.na(idx_same)] <- lang_title[idx_same &!is.na(idx_same)]


### if abstract is "ig" and title is "en", keep "en"or "pt"
lang_final[lang_abstract %in% "ig" & lang_title  %in% c("en", "pt", "es")] <- lang_title[lang_abstract %in% "ig" & lang_title  %in% c("en", "pt", "es")]


# ### if "FLUXNET" or "NPP Tropical Forest:" is in title, keep NA
lang_final[grepl("FLUXNET|NPP Tropical Forest|CarboEurope|NPP Boreal Forest|NPP Multi-Biome", CITATIONS$citation.title)] <- NA


### replace "ig" by NA
lang_final[lang_final%in%"ig"] <- NA

### change to full name
lang_final <- languages_key[lang_final]

## add to CITATIONS ####
CITATIONS$citation.language[my_is.na(CITATIONS$citation.language)] <- ifelse(my_is.na(lang_final), "NAC", lang_final)[my_is.na(CITATIONS$citation.language)]


# replace empty by "NAC ####
CITATIONS$citation.url[my_is.na(CITATIONS$citation.url)] <- "NAC"
CITATIONS$citation.citation[my_is.na(CITATIONS$citation.citation)] <- "NAC"
CITATIONS$citation.abstract[my_is.na(CITATIONS$citation.abstract)] <- "NAC"
CITATIONS$citation.language[my_is.na(CITATIONS$citation.language)] <- "NAC"


# save ####
write.csv(CITATIONS, file = paste0(dirname(getwd()), "/forc/data/ForC_citations.csv"), row.names = F, fileEncoding =  "UTF-8")
