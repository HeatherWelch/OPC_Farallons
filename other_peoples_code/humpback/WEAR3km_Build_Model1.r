##############################################################################################
# Script WEAR3km_Build.r
#
# This script is sourced by the code "WEAR3km_FULL_Build.r" (which is used to develop 
#  and evaluation whale models), and the code  'WEAR_DailyGrids.r'  (which produces
#  the daily grid predictions) from those models
#
# June 28, 2017  -- Karin Forney (modified from Mn_FULL_Build_SingleResponse_1991_2014.r)
# March 13, 2019 -- Karin Forney (modified for WEAR project, to develop finer-scale, 3-km 
#                   models along just the shelf and slope of the West Coast.  Boundary is
#                   in file: Whale_bound_for_modeling.csv (plotted in CCE360_Mn_boundary.srf)
# 
# Script develops GAMs in mcgv using BF 0-5 segments for  CA Study Area
# using all 7 years of CCE SWFSC shipboard data (1991, 1993, 1996, 2001, 2005, 2008, 2014)
# AND the 2009 SOCAL survey data.
#
# In this script, models are built with the survey data within the area boundary above, and 
# predictions are made on individual years. Observed/Predicted ratios for survey years
# are calculated and stored in a matrix. 
#
# In this script, the number of individuals (n*GS) is modeled as the response
# using a Tweedie distribution where the Tweedie parameter is estimated as part
# of the fitting process.
#  
# This script was built to work with EAB's extracted cetacean and ROMS data, using 
# ~3-km on-effort segments in the  file "WEAR_seg_all.csv" that includes ETOPO depth and 
# depth.sd and ROMS & murSST extractions on 3-km grid conducted by Sam Woodman in Dec 2018.
# Grid files include every other day for 2005-2017, and are located are in KAF's 
# directory "C:\KAF\PROJECTS\SERDP-CCmodels\WhalePreyModels\Grids\CCE_3km_Grids\"
#
# The list of predictors includes ROMS daily dynamic and static predictors.  
#
#
##############################################################################################

# Load r packages

library(mgcv)
library(tweedie)
library(Distance)
#  library(dsm)   # To use  rqgam.check instead of gam.check, but not working with R 3.5.2
library(splancs)

############################################################################
#                 DEFINE PREDICT.GAM AND OTHER  FUNCTIONS                  #
############################################################################
#
# ...predict.gam.null.ck.ident.nooff: identity link without offsets
predict.gam.null.ck.ident.nooff <- function(x,p.data,...)
{
  if(length(names(x$coefficients)) == 1)
  {
    p.resp <- rep(as.numeric(x$coefficients[[1]]), nrow(p.data))
  } else {
    p.resp <- predict.gam(x, p.data,...)
  }
}

#...predict.gam.null.ck.log.off: log link with offsets
predict.gam.null.ck.log.off <- function(x,p.data,...)
{
  if(length(names(x$coefficients)) == 1)
  {
    p.resp <- exp(as.numeric(x$coefficients[[1]]) + log(p.data$effort))
  } else {
    p.resp <- predict.gam(x, p.data,...)
  }
}

############################################################################
fn.get.na <- function(data) { 
  TFna <- ( is.na(data$SST)
            | is.na(data$SSTsd) 
            | is.na(data$MLD)
            | is.na(data$MLDsd)
            | is.na(data$SSH)
            | is.na(data$SSHsd)
            | is.na(data$mSST)
            | is.na(data$mSSTsd4)
            | is.na(data$mSSTsd12)
            | is.na(data$depth)
            | is.na(data$depth.sd)
  )
  TFna
} 
############################################################################
# 
#....................USER TASKS.............................
#
# 
#  Choose model name and create model path  for output files (assumes directories themselves 
#  are created in the WEAR3km_FULL_Build.r code)
#
model.name <- "Model1"
model.path <- paste0(model.name,"/")

#
# SET PATH FOR INPUT DATA FILE 
#  KAF Path: 
infile <- "WEAR_seg_all_1991-2014.csv"
seg.data <- read.table(infile,sep=",",header=T)

#
# Read in WEAR shelf/slope study area, so segments can be restricted
# to this region for model building
#  
# WEAR Shelf/Slope Study Area (assumes +/- 0-180 longitudes)
AreaFile<- "Whale_bound_for_modeling.csv"
AreaBound <- read.csv(AreaFile, header=TRUE, na.strings=c("NA",""), 
                      stringsAsFactors = FALSE) 
names(AreaBound)<-c("x","y")
#
# SELECT SPECIES. 
# Pick a species (SpNum) by specifying a number:
# 74 = fin whale (Bp)
# 75 = blue whale (Bm)
# 76 = humpback whale (Mn)

SpecCod  <- c("Bp", "Bm", "Mn")
SpecNums <- c(74, 75, 76)
SpNum    <- 76
ANI.col <-  paste("ANI",SpNum,sep=".")
nSI.col <-  paste("nSI",SpNum,sep=".")
ESW.col <-  paste("ESW",SpNum,sep=".")
gZERO.col <-  paste("gZERO",SpNum,sep=".")

# SELECT SPLINE AND DEGREES OF FREEDOM FOR SMOOTHERS
# 'cs' uses cubic splines (like Splus), 
# 'ts'= thin plate splines ("shrinkage approach" applies additional smoothing penalty)

spline2use <- "ts"  ## "tp" is default for s()
maxdf= 10           ## k=10 is default for s()

# --- Variables included in this data set----# 
# STATIC PREDICTORS: depth - from ETOPO1, depth.sd = slope calculated as SD of depth (9 pixels)
# ROMS PREDICTORS:	 sst.mean, sst.SD, ssh.mean, ssh.SD, ild.mean, ild.SD	
# murSST PREDICTORS: mursst, mursst.sd4, mursst.sd12   (in segment file)
#    Note: these are names analysed_sst.mean, analysed_sst.SD.04, analysed_sst.SD.12 in grid files!
#
# Simplify segment names for modeling 
#   
segnames <- names(seg.data)
segvar   <- segnames[43:51]
modelvar <- c("mSST","mSSTsd4", "mSSTsd12", "SST", "SSTsd","SSH","SSHsd","MLD","MLDsd")
segnames[segnames %in% segvar] <- modelvar
names(seg.data) <- segnames

# convert longitude to 360   --- not needed for CCE study area, all in +/- 180
# seg.data$mlon <- (seg.data$mlon + 360)  
# For species with known or suspected trends, include a year covariate
# Set up below with year 2000 = average (dataset 1991 - 2009)
#
noNA <- which(fn.get.na(seg.data)==F)  # rows without NAs in columns needed for model.
seg.data.nona <- seg.data[noNA, ] 
#
# Include only data for study area of interest and effort in BF 0-5
#
midpts <- data.frame(x=seg.data.nona$mlon,y=seg.data.nona$mlat)
InArea <- which(inout(midpts,AreaBound,bound=TRUE,quiet=TRUE))
seg.data.nona <- seg.data.nona[InArea,]             
seg.data.nona <- seg.data.nona[seg.data.nona$aveBF<6,]
#
#  seg.data.nona <- seg.data.nona[ seg.data.nona$efftyp != "F",]   ##  to REMOVE 2005 fine lines if needed
#
## EXTRACT SPECIES DATA FOR MODELING
#
avegs <- (seg.data.nona[[ANI.col]]/seg.data.nona[[nSI.col]])
avegs[is.na(avegs)] <- 0
ind    <- seg.data.nona[[nSI.col]] * avegs   # Number of individuals (response variable)
effort <- (seg.data.nona[[ESW.col]] * seg.data.nona[[gZERO.col]] * seg.data.nona$dist * 2)  
seg.data.nona <- cbind.data.frame(seg.data.nona, ind, avegs, effort)
rm(ind, avegs, effort) 

seg.data.nona.no0 <- seg.data.nona[seg.data.nona$avegs > 0,]

### SET STUDY AREA (in km^2) AND IDENTIFY GEOGRAPHIC STRATA FOR SPATIAL ASSESSMENT
#    Note:  The area of 598412 provided below is the sum of pixel areas in the file
#    "Grid_Nonrectangle_3km_WEAR_bathy.csv", but need to check whether land overlap is
#    subtracted from nearshore pixels (***I don't think so -- check with Sam ***)
#
Area.size <-  598412   

############################################################################
# BUILD MODELS using all years of data
############################################################################

# Number of animals GAM
ind.gam <- gam(formula= ind ~
                 offset(log(effort)) 
               #     		+ s(SST, bs=spline2use, k=maxdf) 
               #     		+ s(SSTsd, bs=spline2use, k=maxdf) 
               + s(mSST, bs=spline2use, k=maxdf) 
               #     		+ s(mSSTsd4, bs=spline2use, k=maxdf)
               #+ s(mSSTsd12, bs=spline2use, k=maxdf)
               + s(SSH, bs=spline2use, k=maxdf) 
               #     		+ s(SSHsd, bs=spline2use, k=maxdf) 
               + s(MLD, bs=spline2use, k=maxdf) 
               #        + s(MLDsd, bs=spline2use, k=maxdf) 
               + s(depth, bs=spline2use, k=maxdf)
               #+ s(depth.sd, bs=spline2use, k=maxdf)
               #+ s(year, bs = "cs", k = 3)
               # + te(mlon, mlat, bs=spline2use, k=6)  # df = [k*k] - 1
               , family = tw(),
               method="REML", 
               data = seg.data.nona)

############################################################################

