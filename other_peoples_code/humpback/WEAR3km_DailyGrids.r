############################# WEAR3km_DailyGrids.r ##############################################
#
#  This code uses Sam's plotting function code () modified from CalCOFI_WeeklyGrids_Crop_Ind_ROMS.r (w/s modeling project)
#  to create daily grid-based predictions for the CCE study area.  Selected models are set up in
#  separate files (e.g.,"CCE_Build91_14_Ind.r") which are run using a 'source' command.  
#  Note that this program clears the workspace to make sure no pre-existing objects cause problems.
#
# In addition to the source file,the program requires the user to specify two input files: 
#    1) bathydata:  a file with grid lat/lons and corresponding static predictors
#    2) vardatafil:  a set of corresponding files with daily dynamic ROMS predictors:
#                    these files should be in a separate folder, specified in the main loop
#
#   The program creates one main output object, writen to the file species.pred.file, with the 
#   grid pixel lat/lons and daily columns for density.
#   The first eight columns in the output file are: # days with non-NA values (n), average 
#   density, stdev density, lower and upper 90% conf limit using stdev, SE(density), and
#   lower and upper 90% using SE.
#
#   06/29/2017:  Modified by KAF for Whale/Prey modeling in CenNorCA study area)  
#   03/14/2019:  Modified by KAF for WEAR project (Whale entanglement risk assessment)
#   09/04/2020:  Modified by KAF and SMW to aggregate by time period and make plots
#
#
##################################################################################################
# Clear workspace
  rm(list=ls())

  library(splancs)
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tidyr)

#
#  Run the model by sourcing the model-build code
#
   source('WEAR3km_Build_Model1.r')

# Check results to verify
  summary(ind.gam)
  AIC(ind.gam)
  sqrt(mean(residuals.gam(ind.gam,type="response")^2)) #Model error(RMSE) 

# plot(ind.gam,scale=0,residuals=TRUE)
  par(mfrow=c(3,3), oma=c(1,1,1,1), mar=c(4,4,2,2))  #Bottom,Left,Top,Right
  plot(ind.gam,scale=0, shade=TRUE)

#  
# Species codes (SpNum):
# 74 = fin whale (Bp)
# 75 = blue whale (Bm)
# 76 = humpback whale (Mn)
#
  spec <- SpNum  # specified in Build code

# Set up output file location
#
  species.pred.file <- "WEAR3km_dailyPreds.csv" 

# Specify folder for needed files with grid midpoints and static predictors 
#
 gridpath <- "CCE_3km_Grids/"
 fileroot <- "WEAR_3km_"
 gridfile <- "Grid_Nonrectangle_3km_WEAR_bathy.csv"
 outpath <- paste0(model.path,"PredictionGrids/")

#
#  OPTIONAL TASKS:  Change day.start, day.end, and dynamic variable details in loop below
#
############################ END USER TASKS ##################################################
#
# Define predict.gam functions (defined in source code; not needed here)
#
#  Define function to calculate variance and stdev for rows of a data.frame
#  (KAF found these using a Google search on Splus rowStdevs R)  7 Nov 2013
#
 rowVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=TRUE) {
             if (SumSquares) return(rowSums(x^2, na.rm, dims))
             N <- rowSums(!is.na(x), FALSE, dims)
             Nm1 <- if (unbiased) N-1 else N
             if (twopass) {x <- if (dims==0) x - mean(x, na.rm=na.rm) else
                     sweep(x, 1:dims, rowMeans(x,na.rm,dims))}
             (rowSums(x^2, na.rm, dims) - rowSums(x, na.rm, dims)^2/N) / Nm1
  }
  rowStdevs <- function(x, ...) sqrt(rowVars(x, ...))
#
# #########################################################################################
#  Now read / create prediction datasets.  Longitudes are kept on 0-180 scale (124W = -124)
#
  fixeddata <- read.table(gridfile, colClasses="numeric", sep = ",",header=T, stringsAsFactors=F)
  fixeddata$PIXEL <- c(1:nrow(fixeddata))
  fixeddata <- fixeddata[,c(7,1:6)]    
  names(fixeddata) <- c("pixel","mlat","mlon","mlon360","areakm","depth","depth.sd") 
  fixeddata$effort <- rep(1,nrow(fixeddata))
  species.data <- fixeddata[,1:5]
 
#
# change column names from the dynamic predictors to match the model-building
# variable names.  
#
   oceocols <- c("lat","lon","SST", "SSTsd","SSH", "SSHsd","MLD", "MLDsd", "mSST","mSSTsd4", "mSSTsd12")
#
# Set up grid of ROMS input data with appropriate column
#
  start.ymd <- as.Date("2005/01/01")    #as.Date("2005/01/01")
  end.ymd <- as.Date("2020/09/29")      #as.Date("2019/08/14")
  by.incr <- 2
  grid.dates <- c(seq(start.ymd, end.ymd, by=by.incr))

  write.csv(grid.dates,paste0(model.path,"out/WEAR.dates.csv"))  # save dates for reference

  datelbl <- grid.dates            
  datelbl <- gsub("-",".",datelbl)   #This replaces - with . for column names created below
 
################################################################################################
# Specify starting and ending days here.  
# Note that some yrs/days do not have some ROMS variables 

  grid.start <- 1                      # Specify starting and ending DAY for grid predictions
  grid.end   <- length(grid.dates)    #length(grid.dates) 
  denscol <- (grid.end-grid.start)+1

  alldens <- matrix(nrow=nrow(fixeddata),ncol=denscol)   # to be used for average densities below
  colnames(alldens) <- paste("Dens",datelbl[grid.start:grid.end],sep="")

  t1<-Sys.time()
  print(t1)
  
  for (i in grid.start:grid.end)   {        # specify dailies above
 #   for (i in 5)   {        # specify dailies above
      
    vardatafil <- paste(gridpath, fileroot,grid.dates[i],".csv",sep="")  # specify ROMS filenames
#   vardata <- read.table(vardatafil, sep = ",",header=T)
    vardata <- read.table(vardatafil, colClasses="numeric", sep = ",",header=T, stringsAsFactors=F)
    names(vardata) <- oceocols

#  Add year covariate (needed for some species with GAMs including year to capture trends)
     gridYR <- substr(grid.dates[i], 1,4)
     vardata$year <- as.numeric(gridYR)

#  Combine predictors 

    alldata <- cbind(fixeddata,vardata[,c(3:12)])  # specify variables you need for model
    nID <- nrow(alldata)
    TFna <- fn.get.na(alldata)                    # Find rows with NA values in relevant columns 
    ok <- which(TFna==F)                          # fn.get.na defined in BUILD code sourced above
    data4thisrun <- alldata[ok, ] 


  # run predictions and store them somewhere
    p.ind.gam <- predict.gam.null.ck.log.off(ind.gam, data4thisrun, type="response",se.fit=TRUE)  
    
    dens <- rep(NA,nID)         #start with NA's and then merge back into larger data frame for rows
    dens[ok] <- as.numeric(p.ind.gam$fit)    #that were 'ok' (no missing data)

    SE <- rep(NA,nID)
    SE[ok] <- as.numeric(p.ind.gam$se.fit)

# Create output data.frame for this species
    new.names <- c(names(species.data), paste0(spec,".dens.",datelbl[i]),paste0(spec,".se.",datelbl[i]))
    species.data <- cbind(species.data, dens, SE)
#
    names(species.data) <- new.names
    alldens[,i-grid.start+1] <- dens
   
  }  # end of for i loop

  t2<-Sys.time()
  print(t2-t1)

#
# Remove objects that are no longer needed to free up memory
#
#  rm(seg.data,seg.data.nona,seg.data.nona.no0)
#  rm(alldata,vardata,fixeddata,data4thisrun,dens,TFna,ok)
#
########################### GET MEAN, VAR, AND WEEKLY DENSITIES ############
############################## USING THE DAILIES CREATED ABOVE #############
#
# When entire daily species.data frame created, calculate three additional cols 
# with mean, SD and n for individual densites.  Write to output csv file 
# (need to copy headers to the first row so we can use dimnames.write=F to  
# get rid of the stupid rownames)
#
  zval <- qnorm(0.95)

  nrow(alldens)
  avgdens <- rowMeans(alldens, na.rm=T)
  sumdens <- rowSums(alldens, na.rm=T)
  ndens=sumdens/avgdens

# Calculate standard deviation of the monthly densities
  stdevdens <-rowStdevs(alldens, na.rm=T)
  CVstddens <- stdevdens/avgdens
  L90std <-avgdens/exp(zval*sqrt(log(1+(CVstddens^2))))   # qnorm(0.95) gives z for 
  U90std <-avgdens*exp(zval*sqrt(log(1+(CVstddens^2))))   #  2-tailed 90% C.I. 

# Calculate standard error of the mean of the monthly densities
  SEdens <- stdevdens/sqrt(ndens)
  CVSEdens <- SEdens/avgdens
  L90SE <-avgdens/exp(zval*sqrt(log(1+(CVSEdens^2))))   # qnorm(0.95) gives z for 
  U90SE <-avgdens*exp(zval*sqrt(log(1+(CVSEdens^2))))   #  2-tailed 90% C.I. 
#
# Print output file with daily densities and above summary metrics
#
# Create final output data.frame and print to output file
#
  species.data <- cbind(species.data[,1:5], n.Dens=ndens, Avg.Dens=avgdens, 
                      STD.Dens=stdevdens, L90STD.dens=L90std, U90STD.dens=U90std,
                      SE.Dens=SEdens, L90SE.dens=L90SE, U90SE.dens=U90SE, 
                      species.data[,6:ncol(species.data)])
  hdr <- matrix(names(species.data),1,length(species.data))

  filebase <- paste0("WEAR3km_",spec,"_", start.ymd,"to", end.ymd,"_")
  species.pred.file <- paste0(outpath,filebase,"bidaily_dens.csv") 
  species.pred.file.rds <- paste0(outpath,filebase,"bidaily_dens.rds") 
  
  write.table(hdr,species.pred.file, col.names=FALSE,row.names=FALSE, na="NA", sep=",")
  write.table(species.data,species.pred.file, append=TRUE, 
              col.names=FALSE,row.names=FALSE,na="NA", sep=",")
  saveRDS(species.data, file = species.pred.file.rds)
  
  # ====================================================================
  # ====================================================================
  # ====================================================================
#  New steps from Sam's code:
#   1. Aggregate dailies to desired time frame
#   2. Establish plotting colors and break points
#   3. Plot the grids
  

  x.orig <- species.data %>% 
    select(pixel, mlat, mlon, areakm, starts_with("76.dens."))
  
  source("C:/KAF/PROJECTS/SERDP-CCmodels/WhalePreyModels/Mn 3km Models/whale-model-prep/Whalepreds_aggregate.r")
  source("C:/KAF/PROJECTS/SERDP-CCmodels/WhalePreyModels/Mn 3km Models/whale-model-prep/Whalepreds_aggregate_dates.R")

#
# Sam's manual setting up of date range:
#
    # range.dates <- ymd(
    # paste(rep(2005:2018, each = 12), sprintf("%02d", 1:12), "01", sep = "_"), 
    # "2019_01_01"  # make this the day after the end of the date-range.
    # )
    
#  Alternately can use seq function
    num.months <- length(c(2005:2018))*12+1
    range.dates<- seq(as.Date("2005-01-01"), length.out = num.months, by = "month")
    
  
### Average predictions by month, from Jan 2005 to Dec 2018
#  9:18 is part of column name with date
# aggr.level: aggregation level; either "biweekly" , "monthly", or "#day" 
#   (e.g. "5day"). Ignored if range.dates is not NULL
    
  time.period <- "monthly"

  x.aggr <- whalepreds_aggregate(
    x.orig, 5:ncol(x.orig), 9:18, 
    aggr.level =  time.period,                       # "monthly" 
#    range.dates = range.dates, 
    se.calc = FALSE                  #Note: would need to change naming below if se.calc = TRUE
  ) 
  
  # %>% 
#    set_names(c("pixel", "mlat", "mlon", "areakm", 
#               paste("mn", rep(2005:2018, each = 12), sprintf("%02d", 1:12), sep = "_")))
  
  
  
#   
#   
# # ====================================================================
# #  Karin's old code
#   
# #
# # Create weekly averages for Surfer plotting
# #
# # determine how many whole weeks in each annual time period
# 
#   wk.incr <- 14
#   wk.lbl <- paste0(wk.incr,"d")           #BiWkSt
#   week.st.dates  <- c(seq(start.ymd, (end.ymd), by=wk.incr))
#   week.st.dates        
# 
#   weekdens <- species.data[,1:5]
#   names(weekdens) <- names(species.data[,1:5])
#   week.st <- which(grid.dates %in% week.st.dates)
#   col.lbls <- paste0("St",gsub("-",".",week.st.dates),"_",wk.lbl) 
# 
#   for (w in 1:(length(week.st)-1)) {               ###THIS IS NOT WORKING CORRECTLY, BECAUSE w IS ONLY FOR 
#     week.days <- c(week.st[w]:(week.st[w+1]-1))    ### THE BI-DAILIES, SO THERE ARE HALF AS MANY w'S AS
# #    alldens[1:2,week.days]                        ### THERE SHOULD BE BASED ON THE WEEK.ST.DATES  9/19/19-KAF
#     
#     dailies.2wk <- alldens[,week.days]
#     avgdens <- rowMeans(dailies.2wk, na.rm=T)
#     weekdens[[col.lbls[w]]] <- avgdens
#   }
# #   weekdens[325:330,]
# #   edge<-which(is.na(weekdens[,6]))
# #   weekdens<- weekdens[-edge,]
#    
#    
#    outfile1 <- paste0(outpath,filebase,wk.lbl,"_Dens.csv")
#    write.table(weekdens,outfile1,sep=",",row.names=FALSE)
# #
# #  Now write similar  file with  predicted number of whales per pixel.
# #
#   weekwhales <- weekdens
#    for (w in 6:dim(weekwhales)[2]) {
#      weekwhales[,w] <- weekdens[,w]*weekdens$areakm
#   }
#   outfile2 <- paste0(outpath,filebase,wk.lbl,"_Abund.csv")
#   write.table(weekwhales,outfile2,sep=",",row.names=FALSE)

######################### END OF GRID PREDICTIONS CODE ##################################################
#
# Now create plots by calling function in GridPlots.r
#
  source('whale-model-prep/GridPlots.r')
  
  numplots <- dim(x.aggr)[2]
  xlim <- c(-126, -116)
  ylim <- c(32, 49.5)
  
#
# colorRampPalette(rev(brewer.pal(5, "YlGnBu")))  - leftover command from Sam's code
# Set up plotbreakpoints and colorpalette
#
# Jenks breaks determination
#  (Note fisher gives same bt runs faster than jenks)
  
 y.vec <- unname(unlist(na.omit(x.aggr[,5:ncol(x.aggr)])))
  
 #system.time(j1 <- classInt::classIntervals(head(y.vec, 5000), 5, style = "jenks")) #9s
 #system.time(f1 <- classInt::classIntervals(head(y.vec, 5000), 5, style = "fisher")) #.03s

  system.time(d <- classInt::classIntervals(y.vec, 5, style = "fisher")) #1.45s
   d.brks <- d$brks
  d.brks[1] <- 0
  d.brks[length(d.brks)] <- 0.45    # this is just above maximum
  # 

  #breaks <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.45)
  breaks <- d.brks
  pal <- rev(RColorBrewer::brewer.pal(length(breaks)-1, "YlGnBu"))   # c("YlGn", "Blues", "dodgerblue1", ...)
  
#  for (w in 5:numplots) {    # column 5 is the first density column in the file
    for (w in 30:numplots) {    # column 5 is the first density column in the file
      
    plot.dens <-x.aggr[,c(3,2,w)]    
    plot.cols <- c(1:3)
    plot.file <- paste0(model.path,"Plots_jenks_", time.period,"/Mn_Dens_",names(x.aggr[w]))
    plot.title <- paste0(model.name,": Mn_Dens_",names(x.aggr[w]))

    # RColorBrewer::display.brewer.all()
    plot.dens.func(
      plot.file, plot.dens, plot.cols, map.base = NULL, pch = 19, cex = 0.5, 
      col.pal = pal, col.breaks = breaks, 
      main = plot.title, xlim = xlim, ylim = ylim, legend.pos = 4, 
      file.width = 4.5, file.height = 6.2
    ) 
  }  
  
   save.image(file=paste0(model.name,".RData"))
   # to read back in, e.g.:
   # load("MOdel1Yr.RData")
  
  