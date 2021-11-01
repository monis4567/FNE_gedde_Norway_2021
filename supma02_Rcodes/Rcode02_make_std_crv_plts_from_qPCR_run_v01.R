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
# define full path for input directory
wd00_wd01 <- paste(wd00,"/",wd01,sep="")
wd00_wd02 <- paste(wd00,"/",wd02,sep="")
wd00_wd03 <- paste(wd00,"/",wd03,sep="")
# define full path for output directory
wd00_wd04 <- paste(wd00,"/",wd04,sep="")
#Delete any previous versions of the output directory
unlink(wd00_wd04, recursive=TRUE)
#Create a directory to put resulting output files in
dir.create(wd00_wd04)

#see this website on how to only install required packages
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  scales, 
  fields, 
  gplots,
  plyr)
#ReporteRs)



## install the package 'scales', which will allow you to make points on your plot more transparent
#install.packages("scales")
if(!require(scales)){
  install.packages("scales")
  library(scales)
}
library(scales)

#install.packages("fields")
if(!require(fields)){
  install.packages("fields")
  library(fields)
}
library(fields)

## install the package 'gplots', to be able to translate colors to hex - function: col2hex
#install.packages("gplots")
if(!require(gplots)){
  install.packages("gplots")
  library(gplots)
}
library(gplots)

## install the package 'glad', to be able to color using the function 'myPalette'
#install.packages("glad")
#library(glad)

require(graphics)

#get package to read excel files
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

#get package to do count number of observations that have the same value at earlier records:
# see this website: https://stackoverflow.com/questions/11957205/how-can-i-derive-a-variable-in-r-showing-the-number-of-observations-that-have-th
#install.packages("plyr")
if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}
library(plyr)
#read csv with all merged mxpro results
infile01 <- paste(wd00_wd03,"/assmembl_qpcr_runs_FNE.csv",sep="")
df_F01 <- read.csv(infile01, header = TRUE, sep = ",", quote = "\"",
                   dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE)
#read txt with information on samples
infile02 <- paste(wd00,"/FNE_gedde_Norway_samples.csv",sep="")
df_S01 <- read.csv(infile02, header = TRUE, sep = ",", quote = "\"",
                   dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE)
#pad with zero in front
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
# to get FNE number
df_S01$FNEno <- paste("FNE",stringr::str_pad(df_S01$sampl_no, 3, pad = "0"),sep="")
df_S01$FNEno[grepl("\\.02",df_S01$FNEno)] <- gsub("(FNE)([0-9]+)\\.([0-9]+)$","\\10\\2.\\3",df_S01$FNEno[grepl("\\.02",df_S01$FNEno)])

#replace in column
df_S01$buffer_added <- gsub("Tilsette ATL som vanlig referanse","add ATL as usual w heat incub",df_S01$buffer_added)
df_S01$buffer_added <- gsub("Tilsette ATL som vanlig ekstraheres uten oppvarming","add ATL no heat incub",df_S01$buffer_added)
df_S01$buffer_added <- gsub("Tilsette 1 ml RNAgem hoeyt volum RNAgem ","1.0 mL RNAgem",df_S01$buffer_added)
df_S01$buffer_added <- gsub("Tilsette 0.5 ml RNAgem lavt volum RNAgem","0.5 mL RNAgem",df_S01$buffer_added)
#split column
df_incTm <- data.frame(do.call('rbind', strsplit(as.character(df_S01$Incubation_temp_and_period),' ',fixed=TRUE)))
# add back to data frame
df_S01$inctmp <- df_incTm$X3
df_S01$inctime <- paste(df_incTm$X6,df_incTm$X7,sep="_")
# copy the original data frame
df_F02 <- df_F01 
#match between two data frames
df_F02$Fixated  <- df_S01$Fixated[match(df_F02$smplno,df_S01$FNEno)]

df_F02$buffer_added <- df_S01$buffer_added[match(df_F02$smplno,df_S01$FNEno)]
df_F02$inctmp <- df_S01$inctmp[match(df_F02$smplno,df_S01$FNEno)]
df_F02$inctime <- df_S01$inctime[match(df_F02$smplno,df_S01$FNEno)]
df_F02$Type_smpl <- df_S01$Type_smpl[match(df_F02$smplno,df_S01$FNEno)]
#replace with gsub in column names
replc.col.nms <- gsub("[[:punct:]]" ,"",colnames(df_F02))
#replace the old column names with the new column names
colnames(df_F02) <- replc.col.nms

#change column into numeric variable
df_F02$CtdRn=as.numeric(as.character(df_F02$CtdRn))
df_F02$Quantitycopies=as.numeric(as.character(df_F02$Quantitycopies))
colnames(df_F02) <- gsub(" ","_",colnames(df_F02))
colnames(df_F02) <- gsub("\\.","",colnames(df_F02))
#match back the new columns to the original data frame
df_F02$templ.vol2 <- as.numeric(gsub("uL","",df_F02$templvol))

df_F02$smplno2 <- NA
df_F02$smplno2[df_F02$WellType=="Unknown"] <- df_F02$smplno[df_F02$WellType=="Unknown"]
df_F02$smplno2[!df_F02$WellType=="Unknown"] <- df_F02$WellType[!df_F02$WellType=="Unknown"]

#get the unique smpl names for Harbours and WellTypes
unWT <- unique(df_F02$smplno2)
# make a transparent color
transp_col <- rgb(0, 0, 0, 0)
#transp_col <- as.character("#FFFFFF")
unWTnoNA <- addNA(unWT)
col.01<-as.numeric(as.factor(unWT))
  
#make a small dataframe w harbours and standards and numbers assigned, 
#use the col2hex in gplot pacakge to convert the 'red' color name to hex-color
col.02 <- gplots::col2hex(palette(rainbow(length(col.01))))
wt.cols <- cbind(unWT,col.01, col.02)
  
#replace the colour for the standard dilution sample type with the transparent colour
col.03<-replace(col.02, col.01==1, transp_col)
col.04 <- cbind(wt.cols,col.03)
colforwt <- as.data.frame(col.04)
#match to main data frame and add as new color
df_F02$col.06 <- colforwt$col.03[match(df_F02$smplno2, colforwt$unWT)]
####################################################################################
#
# prepare std dilution curve plots for each for species
#
####################################################################################
# get unique species names 
latspecnm <- unique(df_F02$spcabbr)
#identify unique qPCR runs
ls_qPCRnos <- unique(df_F02$qPCRno)
######################################################################################
#   make standard curve plots for each qPCR run
######################################################################################
for (q in ls_qPCRnos)   # for loop start here
{
  print(q)
#}
    qnumber <- gsub("qPCR","",q)
  #define output file and path
    outfilpth <- paste(wd00_wd04,"/",(paste("stdcrv_",q,".pdf",  sep = "")),sep="")
    outfilpth_png <- paste(wd00_wd04,"/",(paste("stdcrv_",q,".png",  sep = "")),sep="")
    # # Exporting PFD files via postscript()           
    # pdf(c(outfilpth)
    #     ,width=(1*1.6*8.2677),height=(1*1.6*2*2.9232))
    # #op <- par(mfrow=c(2,2), # set number of panes inside the plot - i.e. c(2,2) would make four panes for plots
    # op <- par(mfrow=c(1,1), # set number of panes inside the plot - i.e. c(2,2) would make four panes for plots
    #           oma=c(1,1,0,0), # set outer margin (the margin around the combined plot area) - higher numbers increase the number of lines
    #           mar=c(5,5,5,5) # set the margin around each individual plot 
    # )
    # Exporting PNG files via postscript()          
    png(c(outfilpth_png)
    )
        #,width=(1*1.6*8.2677),height=(1*1.6*2*2.9232))
    #op <- par(mfrow=c(2,2), # set number of panes inside the plot - i.e. c(2,2) would make four panes for plots
    op <- par(mfrow=c(1,1), # set number of panes inside the plot - i.e. c(2,2) would make four panes for plots
              oma=c(1,1,0,0), # set outer margin (the margin around the combined plot area) - higher numbers increase the number of lines
              mar=c(5,5,5,5) # set the margin around each individual plot 
    )
    
    #subset based on variable values, subset by species name and by season
    df_F03 <- df_F02[ which(df_F02$qPCRno==q), ]
    #subset to exclude NAs monitored
    df_F03 <- df_F03[!is.na(df_F03$Quantitycopies), ]
    #make the copy numbers numeric
    df_F03$Quantitycopies <- as.numeric(df_F03$Quantitycopies)
    #identify LOD
    lod.id.df<-df_F03[(df_F03$WellType=='Standard'),]
    #replace NAs in standard dilution series with zeroes
    lod.id.df <- lod.id.df[(lod.id.df$Quantitycopies>0) , ]
    #test if the LOD is infinite - in case of no standard curve
    if (is.finite(min(lod.id.df$Quantitycopies))==F) {
      print("no_std_crv")
      lod.val <- 1
    } else {
      lod.val<-min(lod.id.df$Quantitycopies)
      # print(lod.val)
    }
    lod.val2 <- lod.val
    #match LOD to Cq value, exclude NAs, and get max Cq value
    mx.Cq.lod<- max(na.omit(df_F03$CtdRn[lod.val==df_F03$Quantitycopies]))
    # add to the matrix initiated before the loop started
    # see https://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop
    #identify LOQ
    #limit the dataframe to only well type that equals standard
    zc<-df_F03[(df_F03$WellType=='Standard'),]
    #count the occurences of dilution steps - i.e. the number of succesful replicates
    #see this webpage: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
    #zd<-count(zc, "WellName")
    #zd<-dplyr::count(zc, "Quantitycopies")
    zd <- dplyr::count(zc, Quantitycopies)
    #turn this into a dataframe
    ze<-as.data.frame(zd)
    #match the dilution step to the number of occurences -i.e. match between the two dataframes
    no.occ <- ze$n[match(zc$Quantitycopies,ze$Quantitycopies)]
    #add this column with counted occurences to the limited dataframe
    zg <- cbind.data.frame(zc,no.occ)
    #exlude all observations where less than 3 replicates amplified
    zh<-zg[(zg$no.occ>=3),]
    #get the lowest dilution step that succesfully ampllified on all 3 repliactes
    loq.val=min(zh$Quantitycopies)
    loq.val2 <- loq.val
    #Conditionally Remove Dataframe Rows with R
    #https://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
    df_F04<-df_F03[!(df_F03$WellType=='Standard' & df_F03$Quantitycopies<=5),]
    ##  Create a data frame with eDNA
    y.df_F03 <- df_F03$CtdRn
    x.df_F03 <- df_F03$Quantitycopies
    df_F05 <- data.frame( x.df_F03 = x.df_F03, y.df_F03 = y.df_F03 )
    #subset to only include the standard curve points
    # to infer the efficiency of the assay.
    df_F06 <- df_F03[df_F03$WellType=="Standard", ]
    #calculate the covariance
    cov_df_F06 <- cov(df_F06$CtdRn, df_F06$Quantitycopies)
    #calculate the correlation
    cor_df_F06 <- cor(-log10(df_F06$Quantitycopies), df_F06$CtdRn)*100
    rcor_df_F06 <- round(cor_df_F06, 3)
    #begin plot
    plot(
      y.df_F03 ~ x.df_F03,
      data = df_F05,
      type = "n",
      log  = "x",
      las=1, # arrange all labels horizontal
      xaxt='n', #surpress tick labels on x-axis
      yaxt='n', #surpress tick labels on y-axis
      #add a title with bquote
      main=c(bquote('qPCR std crv for'~italic(.('Esox lucius'))
                    #~'('~.(sbs.dknm)~'), '
                    #~'AssayNo'~.("00")~', '
                    ~'qPCRno'~.(qnumber)~', '
                    #~.(eng.seas)
      )),
      xlab="target-eDNA in extract. (copy/qPCR-reaction)",
      ylab="Cycle of quantification",
      xlim = c( 0.234, 0.428*1000000000 ),
      ylim = c( 9.55, 48.446 )
      
    )
    ##  Put grid lines on the plot, using a light blue color ("lightsteelblue2").
    # add horizontal lines in grid
    abline(
      h   = c( seq( 8, 48, 2 )),
      lty = 1, lwd =0.6,
      col = colors()[ 225 ]
    )
    # add vertical lines in grid
    abline(
      v   = c( 
        seq( 0.1, 1, 0.1 ),
        seq( 1e+0, 1e+1, 1e+0 ),
        seq( 1e+1, 1e+2, 1e+1 ),
        seq( 1e+2, 1e+3, 1e+2 ),
        seq( 1e+3, 1e+4, 1e+3 ),
        seq( 1e+4, 1e+5, 1e+4 ), 
        seq( 1e+5, 1e+6, 1e+5 ),
        seq( 1e+6, 1e+7, 1e+6 ),
        seq( 1e+7, 1e+8, 1e+7 ),
        seq( 1e+8, 1e+9, 1e+8 )),
      lty = 1, lwd =0.6,
      col = colors()[ 225 ]
    )
    # add line for LOQ
    abline(v=loq.val, lty=2, lwd=1, col="black")
    text(loq.val*0.7,15,"LOQ",col="black",srt=90,pos=1, font=1)
    # add line for LOD 
    abline(v=lod.val, lty=1, lwd=1, col="red")
    text(lod.val*0.7,22,"LOD",col="red",srt=90,pos=1, font=1)
    # add line for Ct-cut-off
    abline(h=seq(41,100,1000), lty=1, lwd=3, col="darkgray")
    text(10,40.6,"cut-off",col="darkgray",srt=0,pos=3, font=2, cex=1.2)
    ##  Draw the points over the grid lines.
    points( y.df_F03 ~ x.df_F03, data = df_F05, 
            pch=c(24), lwd=1, cex=1.8,
            bg=as.character(df_F03$col.06)
            
    )
    #edit labels on the x-axis
    ticks <- seq(-1, 9, by=1)
    labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
    axis(1, at=c(0.1, 1, 10, 1e+2, 1e+3, 1e+4, 1e+5, 1e+6, 1e+7, 1e+8, 1e+9), pos=8, labels=labels)
    #edit labels on the y-axis
    axis(side=2, at=seq(8, 50, by = 2), las=1, pos=0.1)
    #estimate a model for each STD subset incl below LOQ
    df_F03$x <- df_F03$Quantitycopies
    df_F03$y<- df_F03$CtdRn
    # calculate the log10 for for the Quantitycopies
    df_F03$log10x <- log10((df_F03$Quantitycopies))
    #estimate a linear model 
    logEst.amp_STD <- lm(y~log(x),df_F03)
    # calculate the log10 for for the Quantitycopies
    df_F03$log10x <- log10(df_F03$Quantitycopies)
    #estimate a linear model 
    logEst.amp_STD <- lm(y~log(x),df_F03)
    #estimate a linear model for the log10 values
    # to get the slope
    log10xEst.amp_STD <- lm(y~log10x,df_F03)
    #estimate a model for each STD subset incl below LOQ
    df_F03$x <- df_F03$Quantitycopies
    df_F03$y<- df_F03$CtdRn
    logEst.amp_STD <- lm(y~log(x),df_F03)
    
    #add log regresion lines to the plot
    with(as.list(coef(logEst.amp_STD)),
         curve(`(Intercept)`+`log(x)`*log(x),add=TRUE,
               lty=1))
    #estimate a model for each STD subset for dilution steps above LOQ
    ab.loq.df_F03<-zh # get the previously limited dataframe from identifying LOQ
    ab.loq.df_F03$x <- ab.loq.df_F03$Quantitycopies
    ab.loq.df_F03$y<- ab.loq.df_F03$CtdRn
    logEst.abloqamp_STD <- lm(y~log(x),ab.loq.df_F03) #make a linear model
    
    #get the slope to calculate the efficiency
    slo1 <- log10xEst.amp_STD$coefficients[2]
    slo2 <- as.numeric(as.character(slo1))
    
    intc1 <- log10xEst.amp_STD$coefficients[1]
    intc2 <- as.numeric(as.character(intc1))
    # If log(x) = -1.045
    #Then x = 10^-1.045 = 0.09015711
    #slo3 = 10^slo2
    #Effic <- (-1/slo2)*100
    #Try with perfect efficiency
    #2^3.3219400300021
    #slo2 = -3.3219400300021
    #https://www.gene-quantification.de/efficiency.html
    #qPCR efficiency
    Effic <- (-1+(10^(-1/slo2)))*100
    #amplification factor
    ampF <- 10^(-1/slo2)
    rEffic <- round(Effic,2)
    intc3 <- round(intc2,2)
    slo3 <- round(slo2,2)
    
    #add log regresion lines to the plot
    with(as.list(coef(logEst.abloqamp_STD)),
         curve(`(Intercept)`+`log(x)`*log(x),add=TRUE,
               lty=1, col="red"))
    
    #add 95% confidence intervals around each fitted line
    #inspired from this webpage
    #https://stat.ethz.ch/pipermail/r-help/2007-November/146285.html
    
    #for the first line - with below LOQ
    newx<-seq(lod.val,1e+6,1000)
    prdlogEst.amp_STD<-predict(logEst.amp_STD,newdata=data.frame(x=newx),interval = c("confidence"), 
                               level = 0.95, scale=1 , type="response")
    prd2logEst.amp_STD<- prdlogEst.amp_STD
    lines(newx,prd2logEst.amp_STD[,2],col="black",lty=2)
    lines(newx,prd2logEst.amp_STD[,3],col="black",lty=2)
    
    
    #add 95% conf. intervals for the second line - only above LOQ
    newx<-seq(loq.val,1e+6,100)
    prdlogEst.abloqamp_STD<-predict(logEst.abloqamp_STD,newdata=data.frame(x=newx),interval = c("confidence"), 
                                    level = 0.95, scale=1 , type="response")
    prd2logEst.abloqamp_STD<- prdlogEst.abloqamp_STD
    lines(newx,prd2logEst.abloqamp_STD[,2],col="red",lty=2)
    lines(newx,prd2logEst.abloqamp_STD[,3],col="red",lty=2)
    
    # add a legend for colors on points
    legend(1e+7*0.5,49,
           unique(df_F03$smplno2),
           pch=c(24),
           bg="white",
           #NOTE!! the hex color numbers must be read as characters to translate into hex colors
           pt.bg = as.character(unique(df_F03$col.06)),
           y.intersp= 0.7, cex=0.9)
    
    # add a second legend for types of regression lines
    legend(1000,49,
           c("incl below LOQ","excl below LOQ"),
           #pch=c(24), #uncomment to get triangles on the line in the legend
           cex=0.8,
           bg="white",
           lty=c(1), col=c("black","red"),
           y.intersp= 0.7)
    
    # add a third legend for efficiency and R2
    legend(1e+7*0.5,28,
           c(paste("efficiency: ",rEffic," %",sep=""),
             paste("R2: ",rcor_df_F06,sep=""),
             paste("equation: y=",slo3,"log(x) +",intc3,sep=""),
             paste("Highest Cq at LOD: ",mx.Cq.lod)),
           cex=0.9,
           bg="white",
           y.intersp= 1.0)
    
    # add title for the pdf-page
    mtext(c(paste("Appendix A",q,"."),  sep = ""), outer=TRUE, 
          #use at , adj and padj to adjust the positioning
          at=par("usr")[1]+0.15*diff(par("usr")[1:2]),
          adj=3.4,
          padj=2,
          #use side to place it in te top
          side=3, cex=1.6, line=-1.15)
    
    #apply the par settings for the plot as defined above.
    par(op)
    # end pdf file to save as
    dev.off()  

  } # for loop on qpcr runs end here

#make a file name and and out put file for the complete data frame
outfilpth2 <- paste(wd00_wd04,"/df_w_all_qpcr_runs.csv",sep="")
#write the merged data frame to a csv file
write.table(df_F02,outfilpth2,sep=";", col.names=T)
#________________________________________________________________________________
#________________________________________________________________________________
