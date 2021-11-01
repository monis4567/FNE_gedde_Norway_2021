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
wd00 <- "/home/hal9000/FNE_gedde_Norway_2021"
setwd(wd00)
#define sub directories
wd01 <- "supma01_inp_raw_qcpr_txt"
wd02 <- "supma02_Rcodes"
#define directory with output flies
wd03 <- "supma03_assmebl_csv_from_qPCR_runs"
wd04 <- "supma04_std_crv_plots_from_qPCR_runs"
wd05 <- "supma05_plots_w_eDNA_lvls_per_smpl"
# define full path for input directory
wd00_wd01 <- paste(wd00,"/",wd01,sep="")
wd00_wd02 <- paste(wd00,"/",wd02,sep="")
wd00_wd03 <- paste(wd00,"/",wd03,sep="")
# define full path for output directory
wd00_wd04 <- paste(wd00,"/",wd04,sep="")
wd00_wd05 <- paste(wd00,"/",wd05,sep="")
#Delete any previous versions of the output directory
unlink(wd00_wd05, recursive=TRUE)
#Create a directory to put resulting output files in
dir.create(wd00_wd05)

#define in file and path
infile03_pth <- paste(wd00_wd04,"/","df_w_all_qpcr_runs.csv",sep="")
#read in file
df_F07 <- read.table(infile03_pth, header = TRUE, 
                     stringsAsFactors = TRUE,fill = TRUE,sep=";")
#define path and file with concentration measurements
infile04_pth <- paste0(wd00,"/table01_conc_measurements_on_Extractions.txt")
#read in file
df_c01 <- read.table(infile04_pth, header = TRUE, 
                     stringsAsFactors = TRUE,fill = TRUE,sep="\t")


#read in packages for making plots
library(ggplot2)
library(readr)
#limit dataframe to only include samples
# i.e. exclude standards
df_F08 <- df_F07[(!df_F07$smplno2=="Standard"),]
#df_F08 <- df_F08[(df_F08$smplno2=="NTC"),]
#replace NAs with zeroes
df_F08$Quantitycopies[is.na(df_F08$Quantitycopies)] <- 0
#convert quantity copies to log 10 scale
df_F08$Quant_cop_l10 <- log10(df_F08$Quantitycopies+1)
df_F08$Fixated_tp <- NA
df_F08$Fixated_tp <- df_F08$Fixated 
df_F08$Fixated_tp <- as.character(df_F08$Fixated_tp)
df_F08$Fixated_tp[df_F08$smplno2=="NTC"] <- as.character("Neg Control")
df_F08$Fixated_tp[df_F08$smplno2=="NEK"] <- as.character("Neg Control")

df_F08$Typesmpl <- as.character(df_F08$Typesmpl)
df_F08$Typesmpl[df_F08$smplno2=="NEK"] <- as.character(df_F08$smplno2[df_F08$smplno2=="NEK"])
df_F08$Typesmpl[df_F08$smplno2=="NTC"] <- as.character(df_F08$smplno2[df_F08$smplno2=="NTC"])


#https://stackoverflow.com/questions/31630045/understanding-boxplot-with-jitter
p01 = ggplot(df_F08, aes(x=smplno2, y=Quant_cop_l10)) +
  geom_point(aes(fill=Typesmpl), size=3, shape=21, colour="grey20") +
  geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") +
  facet_wrap(~Fixated_tp) +
  facet_grid(rows= vars(Fixated_tp)) +
  # scale_fill_manual(values=alpha(
  #   c(cl03),
  #   c(0.7)
  # ))+
  coord_flip() +
  labs(title="Not Jittered")
#p01

#adjust tick labels
#http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
p01a <- p01 + theme(axis.text.x = element_text(face="plain", color="black", 
                                     size=11, angle=0),
          axis.text.y = element_text(face="plain", color="black", 
                                     size=4, angle=0))

# Add titles
# see this example: https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/
p01t <- p01a + labs(title = "qPCR on eDNA from Esox lucius")#,
#change axis labels
p01t <- p01t + xlab("sample no") + ylab("log10(eDNA copy number per reaction)")
#p01t

library(gridExtra)
#getwd()
outfil01 <- paste(wd00_wd05,"/","qPCR_on_Esox_lucius.png",sep="")

bSaveFigures=T
if(bSaveFigures==T){
  ggsave(p01t,file=outfil01,width=210,height=297,
         units="mm",dpi=300)
}

#
#_____________________________________________________________________________
# Compare concentrations measured after extractions with eDNA levels
#_____________________________________________________________________________

colnames(df_c01) <- gsub("ø","oe",colnames(df_c01))

df_c01$Proevenr2 <- as.numeric(as.character(df_c01$Proevenr))
df_c01$Proevenr3 <- sprintf("%03d",df_c01$Proevenr2)
#edit sample names in concentration file
df_c01$FNEno <- gsub("^(.*)","FNE\\1",df_c01$Proevenr3)

#colnames(df_F08)
df_F08$konc.i.ng.uL <-df_c01$konc.i.ng.uL[match(as.character(df_F08$smplno2), df_c01$FNEno)]
is.na(df_F08$konc.i.ng.uL) <- 0
df_F08$Fixated_tp.Typesmpl <- paste(df_F08$Fixated_tp,df_F08$Typesmpl,sep=".")
#
df_F08$konc.i.ng.uL[grepl("RNA",df_F08$Fixated_tp.Typesmpl)]

#colnames(df_F07)
p <- ggplot(df_F08,aes(x=konc.i.ng.uL,
                       y=Quant_cop_l10,
                       col=Fixated_tp.Typesmpl)) + 
          geom_point(size=3)
#p

# Add titles
# see this example: https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/
p02t <- p + labs(title = "qPCR on eDNA from Esox lucius nad conc on extractions")#,
#change axis labels
p02t <- p02t + xlab("conc. in extraction (ng/uL)") + ylab("log10(eDNA copy number per reaction)")
#p02t
#getwd()
outfil02 <- paste(wd00_wd05,"/","qPCR_on_Esox_lucius_w_conc.png",sep="")
#also save teh second plot
bSaveFigures=T
if(bSaveFigures==T){
  ggsave(p02t,file=outfil02,width=210,height=297,
         units="mm",dpi=300)
}
