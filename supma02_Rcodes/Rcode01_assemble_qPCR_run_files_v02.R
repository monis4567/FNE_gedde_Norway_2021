#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# 
#
# “Monitoring  
#  environmental DNA from European pike (Esox lucius)
#  in freshwater samples collected in Norway and extracted
#  with different extraction protocols”
#
# Authors: Steen Wilhelm Knudsen, 
#

# Change the working directory to a path on your own computer , and run
# the individual parts below to reproduce the diagrams presented in the paper
#
# All input data required needs to be available as text-files in 
# a separte directory in this R project

# Occassionally the code will have difficulties producing the correct diagrams,
# if the packages and libraries are not installed.
# Make sure the packages are installed, and libraries are loaded, if the R-code
# fails in producing the diagrams.
#
#________________IMPORTANT!!_________________________________________________#
# (1)
#You have to change the path to the working directory before running this code
#
# (2)
# The 3 data input files:
# "DL_dk_specs_to_latspecs.xls"
# "DL_harbour_and_pos_water_samples.xls"
# "outfile01_merged_csv_files_from_mxpro.csv"
#
#
# must be located in the same working directory
#
#This code is able to run in:
#   > R.Version()
# $platform
# [1] "x86_64-pc-linux-gnu"
# 
# $arch
# [1] "x86_64"
# 
# $os
# [1] "linux-gnu"
# 
# $system
# [1] "x86_64, linux-gnu"
# 
# $status
# [1] ""
# 
# $major
# [1] "4"
# 
# $minor
# [1] "1.1"
# 
# $year
# [1] "2021"
# 
# $month
# [1] "08"
# 
# $day
# [1] "10"
# 
# $`svn rev`
# [1] "80725"
# 
# $language
# [1] "R"
# 
# $version.string
# [1] "R version 4.1.1 (2021-08-10)"
# 
# $nickname
# [1] "Kick Things"
#____________________________________________________________________________#


#remove everything in the working environment, without a warning!!
rm(list=ls())
########################################################################################
# set working directory
wd00 <- getwd()
#wd00 <- "/Users/steenknudsen/Documents/Documents/MS_amphibian_eDNA_assays/MS_suppm_amphibia_eDNA"
wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/fiske_eDNA_200130/FNE_gedde_Norway_2021"
setwd(wd00)
#define sub directories
wd01 <- "supma01_inp_raw_qcpr_txt"
wd02 <- "supma02_Rcodes"
#define directory with input flies
wd03 <- "supma03_assmebl_csv_from_qPCR_runs"
# define full path for input directory
wd00_wd01 <- paste(wd00,"/",wd01,sep="")
wd00_wd02 <- paste(wd00,"/",wd02,sep="")
# define full path for output directory
wd00_wd03 <- paste(wd00,"/",wd03,sep="")
#Delete any previous versions of the output directory
unlink(wd00_wd03, recursive=TRUE)
#Create a directory to put resulting output files in
dir.create(wd00_wd03)

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

# The MxPro qPCR machine can also prepare '.txt' files
# Which are used in the section below

# The BioRad qPCR machine can also generate '.csv' files that are similar in setup
# The iteration over files here below can be adjusted to be a match
# with the '.csv' file output generate by the BioRad qPCR machine
##########################################################################################

#list all files in wd - all the xls-files for which you want to prepare plots from 
ls.fl01 <- list.files(wd00_wd01)
#make a variable with the element you want to search for
id1 <- "Text"
# grep for this variable in the list -  see this example: https://stackoverflow.com/questions/35880242/r-selecting-element-from-list
# because the MxPro 3005 qPCR machine by default prepares text reports with the
# postfix: ' -  Text Report', I thought it would be convenient to grep for the 
# "Text" bit. If the 
ls.fl01.txt <- ls.fl01[grep(paste0(id1), ls.fl01)]

# https://stackoverflow.com/questions/46305724/merging-of-multiple-excel-files-in-r
# https://medium.com/@niharika.goel/merge-multiple-csv-excel-files-in-a-folder-using-r-e385d962a90a
# https://stackoverflow.com/questions/54474404/how-to-detect-time-when-reading-from-an-excel-sheet-using-r
library(openxlsx)
library(readxl)
#__________________________________________________________________________________________
# start : part 01 : iterate over all text report files from qPCR runs
# to make csv files 
#__________________________________________________________________________________________
# iterate over files to write csv files
for (file in ls.fl01.txt)
{
  #print(file)}
  #use gsub to replace in the filename
  fl01 <- gsub(".txt",".csv", file)
  fl02 <- gsub("-","_", file)
  fl03 <- gsub(" ","_", fl02)
  fl03 <- gsub("__","_", fl03)
  fl03 <- gsub("__","_", fl03)
  fl04 <- gsub("õ","oe", fl03)
  fl04 <- gsub("õ","oe", fl04)
  fl04 <- gsub("ù","u", fl04)
  fl04 <- gsub("ù","aa", fl04)
  fl04 <- gsub("Ü","aa", fl04)
  fl04 <- gsub(",","", fl04)
  fl04 <- gsub("ë","ae", fl04)
  fl04 <- gsub("õ","oe", fl04)
  fl04 <- gsub("\\.txt","\\.csv", fl04)
  #remove non ASCII characters
  #https://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
  Encoding(fl04) <- "latin1"  # (just to make sure)
  fl04 <- iconv(fl04, "latin1", "ASCII", sub="")
  # read in the txt file , it will become a tibble
  infile <- paste(wd00_wd01,"/",file,sep="")
  tibl_txt <- read.table(infile, sep="\t", header = TRUE)
  # make the tibble a data frame
  df_txtf01 <- as.data.frame(tibl_txt, stringsAsFactors=FALSE)
  #replace in the column names
  colnames(df_txtf01) <- gsub(" ","_",colnames(df_txtf01))
  colnames(df_txtf01) <- gsub("\\(","",colnames(df_txtf01))
  colnames(df_txtf01) <- gsub("\\)","",colnames(df_txtf01))
  #modify column names to make them identical
  colnames(df_txtf01) <- gsub("\\.\\.dR\\.","\\.\\.dRn\\.",colnames(df_txtf01))
  #get the filename without the text postfix
  fln_wtx <- gsub("\\.txt","",file)
  # add a column with the filename
  df_txtf01$filename <- fln_wtx
  #remove not needed column
  df_txtf01$X <- NULL
  # paste a path and a file name together to write the csv file to
  csvf01 <- paste(wd00_wd03,"/",fl04,sep="")
  # write the csv file to the path
  write.csv(df_txtf01,file = csvf01, quote=TRUE)
}
#__________________________________________________________________________________________
# end : part 01 : iterate over all excel files to make csv files 
#__________________________________________________________________________________________

#__________________________________________________________________________________________
# start : part 02 : iterate over all  csv files prepared above
# to merge them in to a single data frame
#__________________________________________________________________________________________

ls.fl02 <- list.files(wd00_wd03)
#make a variable with the element you want to search for
id1 <- "csv"
#grep for the "csv"
ls.fl02.csv <- ls.fl02[grep(paste0(id1), ls.fl02)]
#make a list to hold csv files read in
ls.df_csv03 <- list()
#
i <- 1
#iterate over csv files prepared
for (file in ls.fl02.csv)
{
  #print the file name
  #print(file)
  infile <- paste(wd00_wd03,"/",file,sep="")
  #read in as a tibble
  tbl_csv <- read.table(infile,sep=",",header=TRUE,stringsAsFactors = FALSE )
  df_csv <- as.data.frame(tbl_csv, stringsAsFactors = FALSE)
  #add data frame to list of data frames
  ls.df_csv03[[i]] <- df_csv
  # add to the increasing number for next iteration
  i <- i+1
}

#make the nested list a data frame
df_csv03 <- as.data.frame(do.call(rbind, ls.df_csv03))
#combine the dataframes in to one
#https://stats.stackexchange.com/questions/244486/rbind-for-dataframes-with-different-number-of-rows
df_M04 <- plyr::rbind.fill(ls.df_csv03)
#split the column with filenames
fnm.spl <- data.frame(do.call('rbind', strsplit(as.character(df_M04$filename),'_',fixed=TRUE)))
#add back to main data frame
df_M04$qPCRno       <- fnm.spl$X1
df_M04$ProjectNm    <- fnm.spl$X2
df_M04$AssIDNo      <- fnm.spl$X3
df_M04$spcabbr      <- fnm.spl$X4
df_M04$spcabbr      <- fnm.spl$X5
df_M04$qpcrmach     <- fnm.spl$X6
df_M04$qpcrrundate  <- fnm.spl$X7
#remove file name column
df_M04$filename <- NULL

#split the column with Well.Name
wllnm.spl <- data.frame(do.call('rbind', strsplit(as.character(df_M04$Well.Name),'_',fixed=TRUE)))
df_M04$replno   <- wllnm.spl$X1
df_M04$smplno   <- wllnm.spl$X2
df_M04$templvol <- wllnm.spl$X3
#remove unneeded column
df_M04$Well.Name <-  NULL
#define output file and path 
outfl.csv <-  paste(wd00_wd03,"/assmembl_qpcr_runs_FNE.csv",sep="")
#write the csv file
write_csv(df_M04,outfl.csv)
#head(df_M04,12)

#__________________________________________________________________________________________
# end : part 02 : iterate over all  csv files prepared above
# to merge them in to a single data frame
#__________________________________________________________________________________________


#