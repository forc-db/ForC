######################################################
# Purpose: Add D.precedence.measurement.ID and assign in it the measurement.ID of the record that D.precendence in the D.group. to run only once.
# Developped by: Valentine Herrmann - HerrmannV@si.edu in April 2018
# R version 3.4.4 (2018-10-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(lubridate)


# Load data ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Add D.precedence.measurement.ID and assign in it the measurement.ID of the record that D.precendence in the D.group ####

## Add D.precedence.measurement.ID ####
names(MEASUREMENTS) <- gsub("precedence.measurement.ID", "D.precedence.measurement.ID", names(MEASUREMENTS))

## Fill D.precedence.measurement.ID with "NAC" when records are independant ####
MEASUREMENTS[MEASUREMENTS$conflicts %in% "I",]$D.precedence.measurement.ID <- "NAC"

all.groups <- unique(unlist(strsplit(MEASUREMENTS$D.group, ",")))
all.groups <- all.groups[!my_is.na(all.groups)]

multiple.D.precedence.in.one.same.D.group <- NULL
no.D.precedence.in.one.D.group <- NULL

for (D.group in all.groups) {
  # we want to look for the group exactly, not a group that would include the group Id but be actually a bigger name... 
  # so we want to look for this patter: "(^D.group&)|(^D.group,)|(,D.group,)|(,D.group$)
  pattern.D.group <- paste0("(^",D.group, "$)|(^",D.group, ",)|(,", D.group, ",)|(,", D.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.D.group, MEASUREMENTS$D.group), ]
  D.precedence.measurement.ID <- X$measurement.ID[X$D.precedence %in% 1]
  
  if(length(D.precedence.measurement.ID) == 0) no.D.precedence.in.one.D.group <- c(no.D.precedence.in.one.D.group, D.group)
  if(length(D.precedence.measurement.ID) > 1) multiple.D.precedence.in.one.same.D.group <- c(multiple.D.precedence.in.one.same.D.group, D.group)
  if(length(D.precedence.measurement.ID) == 1)  MEASUREMENTS[grepl(pattern.D.group, MEASUREMENTS$D.group), ]$D.precedence.measurement.ID <- D.precedence.measurement.ID
}

for (D.group in multiple.D.precedence.in.one.same.D.group) {
  # we want to look for the group exactly, not a group that would include the group Id but be actually a bigger name... 
  # so we want to look for this patter: "(^D.group&)|(^D.group,)|(,D.group,)|(,D.group$)
  pattern.D.group <- paste0("(^",D.group, "$)|(^",D.group, ",)|(,", D.group, ",)|(,", D.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.D.group, MEASUREMENTS$D.group), ]
  # print(X)
  # readline("press [enter]")
}

for (D.group in no.D.precedence.in.one.D.group) {
  # we want to look for the group exactly, not a group that would include the group Id but be actually a bigger name... 
  # so we want to look for this patter: "(^D.group&)|(^D.group,)|(,D.group,)|(,D.group$)
  pattern.D.group <- paste0("(^",D.group, "$)|(^",D.group, ",)|(,", D.group, ",)|(,", D.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.D.group, MEASUREMENTS$D.group), ]
  # print(X)
  # readline("press [enter]")
}


MEASUREMENTS[is.na(MEASUREMENTS$D.precedence.measurement.ID) & !is.na(MEASUREMENTS$D.group),]
table(MEASUREMENTS[is.na(MEASUREMENTS$D.precedence.measurement.ID) & !is.na(MEASUREMENTS$D.group),]$loaded.by)

MEASUREMENTS[is.na(MEASUREMENTS$D.precedence.measurement.ID) & !is.na(MEASUREMENTS$D.group) & MEASUREMENTS$loaded.by %in% "Mohammad Moein Zavarehee Azimi",]

MEASUREMENTS[is.na(MEASUREMENTS$D.precedence.measurement.ID) & !is.na(MEASUREMENTS$D.group) & MEASUREMENTS$loaded.by %in% "Abby Ferson",]

# SAVE ####
write.csv(MEASUREMENTS, "data/ForC_measurements.csv", row.names = F)

