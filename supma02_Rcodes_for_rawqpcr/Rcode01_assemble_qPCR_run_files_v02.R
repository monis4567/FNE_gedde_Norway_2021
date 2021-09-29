

#remove everything in the working environment, without a warning!!
rm(list=ls())
########################################################################################
# set working directory
wd00 <- getwd()
#wd00 <- "/Users/steenknudsen/Documents/Documents/MS_amphibian_eDNA_assays/MS_suppm_amphibia_eDNA"
wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fiske_eDNA_200130/FNE_gedde_Norway_2021"
setwd (wd00)
getwd()
wdin01.1 <- "supma01_inp_raw_qcpr_txt"
wdin02.1 <- "supma02_Rcodes_for_rawqpcr"
#define directory with input flies
wdout01.2 <- "suppma03_ampl_plots_from_qPCR_runs"
# define full path for input directory
inpfdir01 <- paste(wd00,"/",wdin01.1,sep="")
wd <- inpfdir01
# define an outout directory
wdout02.1 <- wdout01.2
wdout02.2 <- paste(wd00,"/",wdout02.1,sep="")
#install packages
#get readxl package
if(!require(readxl)){
  install.packages("readxl")
}  
library(readxl)
#get ggplot package
if(!require(ggplot2)){
  install.packages("ggplot2")
}  
library(ggplot2)

#get pdp package
if(!require(pdp)){
  install.packages("pdp")
}  
library(pdp)

##########################################################################################
# begin -  Function to fill NAs with previous value
##########################################################################################
#fill NAs with latest non-NA value
#http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/
#https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value

fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  
  x
}
##########################################################################################
# end -  Function to fill NAs with previous value
##########################################################################################

##########################################################################################
# begin - install packages to be able to do the ggplot below
##########################################################################################
#get tidyverse package
if(!require(tidyverse)){
  install.packages("tidyverse")
}  
library(tidyverse)

#get broom package
if(!require(broom)){
  install.packages("broom")
}  
library(broom)

#get mgcv package
if(!require(mgcv)){
  install.packages("mgcv")
}  
library(mgcv)

#get tibble package
if(!require(tibble)){
  install.packages("tibble")
}  
library(tibble)

#library(tidyverse)
#library(broom)
#library(mgcv)  #For the gam model
##########################################################################################
# end - install packages to be able to do the ggplot below
##########################################################################################

##########################################################################################
# Note about the input files for this code
##########################################################################################
# The excel files are prepared in the MxPro software as individual amplification plots
# and exported  from MxPro as individual excel spreadsheets
# all spreadsheets can then be zipped together, and transfered to your own computer
# unzip the zip file with all xls-files in a folder that also works as working directory
##########################################################################################

#list all files in wd - all the xls-files for which you want to prepare plots from 
ls.fl01 <- list.files(paste(wd00,"/",wdin01.1,sep=""))
#make a variable with the element you want to search for
id1 <- "txt"
#grep for this variable in the list -  see this example: https://stackoverflow.com/questions/35880242/r-selecting-element-from-list
ls.fl01.txt <- ls.fl01[grep(paste0(id1), ls.fl01)]
