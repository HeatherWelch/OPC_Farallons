#######Get_Env_Data B
# Get data sequence number two : 
# just download the data from the thredds server via opendap
# Adapted from EcoCast by Heather Welch (UCSC/NOAA)

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
# path="/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization"
path="/Users/EcoCast/Dropbox/OPC_Farallons/operationalization"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
# source_path="/Users/heatherwelch/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V1"
source_path="/Users/EcoCast/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V2"

## path to copernicus toolbox
# path_copernicus_marine_toolbox = "/Users/heatherwelch/miniforge3/envs/copernicusmarine/bin/copernicusmarine"
path_copernicus_marine_toolbox = "/Users/EcoCast/miniforge3/envs/copernicusmarine/bin/copernicusmarine"

############# ----------------------------> End ################

Get_Env_Data_B_batch=function(path,source_path,date_range){
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)
  source(paste0(source_path,"/utils.R"))
  
  envdir=glue("{path}/daily_prediction_layers")
  outdir <- glue("{path}/end_products_final")
  staticdir=glue("{path}/static_variables")
  # temp=glue("{path}/raw_roms_data")
  moddir=glue("{path}/models")
  intermediatedir=glue("{path}/intermediate")
  
  # flyersdir=glue("{outdir}/flyers")
  # rastersdir=glue("{outdir}/rasters")
  # mapssdir=glue("{outdir}/maps")
  # latestdir=glue("{outdir}/latest")
  # latestweekdir=glue("{outdir}/latest_week")
  # latestweeksmootheddir=glue("{outdir}/latest_week_smoothed")
  
  ############ 2. Define time and dynamic directories
  
  ############ 3. define functions
  
  handle_the_data=function(get_date,var,template,save_var,finaldir){ 
    # tryCatch(
    #  expr= {
    print(var)
    
    ref_date <- dmy('02-01-2011')
    new_date <- as.Date(get_date)
    days <- as.numeric(difftime(new_date, ref_date))
    
      my_url = glue("https://oceanmodeling.ucsc.edu/thredds/dodsC/ccsra_2016a_phys_agg_derived_vars/fmrc/CCSRA_2016a_Phys_ROMS_Derived_Variables_Aggregation_best.ncd?{var}[{days}:1:{days}][0:1:180][0:1:185],lat_rho[0:1:180][0:1:185],lon_rho[0:1:180][0:1:185],time[0:1:1]")
      nc.data=nc_open(my_url)
    print("grabbing dimensions and variables")
    lat <- ncvar_get(nc.data,'lat_rho')
    lon <- ncvar_get(nc.data,'lon_rho')
    nrows <- length(lat); ncols <- length(lon)
    
   
    tmp.array <- ncvar_get(nc.data, var)
    fillvalue <- ncatt_get(nc.data, var, "_FillValue")
    print("creating matrix for day of interest")
    
    dat1=list()
    dat1$x=lon
    dat1$y=lat
    dat1$z=t(tmp.array)
    
    r <-raster(
      dat1$z,
      xmn=range(dat1$x)[1], xmx=range(dat1$x)[2],
      ymn=range(dat1$y)[1], ymx=range(dat1$y)[2], 
      crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )
    r2=flip(r,2)
    
    #Force to template res
    r3 <- raster::resample(r2, template)  
    extent(r3)=extent(template)
    # return(r3)
    writeRaster(r3,glue("{finaldir}/{save_var}.grd"),overwrite=T)
    
    ## different resolutions for humpback ILD
    if(var=="ild_05"){
      r3_hump <- raster::resample(r2, template_humpback,method="ngb")  
      extent(r3_hump)=extent(template_humpback)
      # return(r3)
      writeRaster(r3_hump,glue("{finaldir}/{save_var}_humpback.grd"),overwrite=T)
    }
    
    ## different resolutions for humpback SSH and bias correction
    if(var=="ssh"){
      r3_hump <- raster::resample(r2, template_humpback,method="ngb") + 0.035
      extent(r3_hump)=extent(template_humpback)
      # return(r3)
      writeRaster(r3_hump,glue("{finaldir}/{save_var}_humpback.grd"),overwrite=T)
    }
    
    ## different SD resolutions for anchovy and epac
    if(var=="sst"|var=="ssh"){
      rasSD07=focal(r3,w=matrix(1,nrow=7,ncol = 7),fun=sd,na.rm=T) ## create SD # .07 for barb
      writeRaster(rasSD07,glue("{finaldir}/{save_var}_sd_.07.grd"),overwrite=T)
      rasSD03=focal(r3,w=matrix(1,nrow=3,ncol = 3),fun=sd,na.rm=T) ## create SD # .03 for meg
      writeRaster(rasSD03,glue("{finaldir}/{save_var}_sd_.03.grd"),overwrite=T)
    }
    
  }
  examine_times <- function(time_range, get_date){
    ## time_range = vector; the min and max dates returned by cmems_time() for productID
    ## get_date = vector; a 'Date' object for the date of interest for environ data
    
    ntimes <- difftime(time_range[2], time_range[1], units = "days") %>% 
      as.numeric()
    times <- seq(time_range[1], time_range[2], by = "1 day")
    
    nearest_date_position <- which.min(abs(times - as_date(get_date)))
    nearest_date <- times[nearest_date_position] 
    how_different <- difftime(as_date(get_date), nearest_date, units = "days") %>%  
      as.numeric()
    notation_date <- nearest_date	
    
    return(list(nearest_date_position, nearest_date, how_different, notation_date))
    
  }
  getDepthPosition <- function(conn,depth){
    # returns nearest depth index to given depth from depth axis in an opendap connection
    #parameters
    ## conn: netcdf file or remote opendap connection, obtained with nc_open
    ## depth: depth to extract in same units as in the file
    ndepth=conn$dim$depth$len
    depths=conn$dim$depth$vals
    nearest_depth_position=(which.min(abs(depths-depth)))
    return(nearest_depth_position)
  }
  
  ############ 4. define global objects
  template=raster(glue("{path}/static_variables/template.grd"))
  template_humpback=read.csv(glue("{staticdir}/humpback/Grid_Nonrectangle_3km_WEAR_bathy.csv"))[, c("lon180", "lat")] %>% 
    rasterFromXYZ()
  crs(template_humpback)=crs(template)
  box=c(-134,-115.5,30,48) ## xmin,xmax,ymin,ymax
  ############ 5. download data
  
  for(date in date_range){
    get_date=date
    finaldir=glue("{envdir}/{get_date}")
      if(length(list.files(finaldir))!=38){
      print(glue("ROMS data don't exist for {get_date}, starting download"))
    print(get_date)

    tryCatch(
      expr = {
        
    print("**************************************************************************************")
    print(paste0("Starting script batch Get_Env_Data_B_batch.R,"," Time is ",Sys.time()))
    print(date)
    tryCatch(
      expr = {
  
        ## humpback sst
        if(!file.exists(glue("{finaldir}/sst_humpback.grd"))){
          print("humpback sst doesn't exist, downloading")
        sst_url=glue("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.csv?analysed_sst%5B({get_date}T09:00:00Z):1:({get_date}T09:00:00Z)%5D%5B(32):1:(49)%5D%5B(-127):1:(-116)%5D")
        file = glue("{intermediatedir}/netcdfs/jplMURSST41_{get_date}.csv")
        print(paste("Beginning download of humpback sst. Placing it in a temp directory: ",intermediatedir,sep=""))
        f = CFILE(file,mode="wb")
        a=curlPerform(url=sst_url,writedata=f@ref,noprogress=FALSE, .opts = RCurl::curlOptions(ssl.verifypeer=FALSE))
        close(f)
        sst=read.csv(file)[, c("longitude", "latitude","analysed_sst")] %>% 
          rasterFromXYZ() 
        crs(sst)=crs(template_humpback)
        sst2=raster::resample(sst,template_humpback)
        extent(sst2)=extent(template_humpback)
        writeRaster(sst2,glue("{finaldir}/sst_humpback.grd"),overwrite=T)
        }
      },
      error = function(e){
        message(glue("ERDDAP sst not available {get_date}"))
        print(e)
      }
    )
      
    tryCatch(
      expr = {
        
        ## anchovy chl
        #get chl ####
        if(!file.exists(glue("{finaldir}/l.chl.grd"))){
          print("chl doesn't exist, downloading")
          productId = "cmems_obs-oc_glo_bgc-plankton_nrt_l4-gapfree-multi-4km_P1D"
          variable <- c("CHL")
          out_name=glue("{productId}_{variable}_{get_date}")
          
          # Query time range for productID
          time_range <- cmems_time(productID = productId,
                                   path_copernicus_marine_toolbox = path_copernicus_marine_toolbox)
          
          # Store time vars as objects
          dates <- examine_times(time_range = time_range, get_date = get_date)
          nearest_date_position=dates[[1]];nearest_date=dates[[2]];how_different=dates[[3]];notation_date=dates[[4]]
          
        if (how_different < 8 & how_different >= 0){
          command <- glue("{path_copernicus_marine_toolbox} subset -i {productId} -t {nearest_date} -T {nearest_date} --variable {variable} -o {intermediatedir} -f {out_name} --force-download")   
          system(command, intern = TRUE)
          
          r=raster(glue("{intermediatedir}/{out_name}.nc"))
          r1=r^.25 ## barb's math
          r2 <- raster::resample(r1, template)  
          extent(r2)=extent(template)
          writeRaster(r2,glue("{finaldir}/l.chl.grd"),overwrite=T)
          
        } else if (how_different < 0) {
          print("Downloading from multi-year product instead of NRT")
          productId = "cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D"
          out_name=glue("{productId}_{variable}_{get_date}")
          
          command <- glue("{path_copernicus_marine_toolbox} subset -i {productId} -t {get_date} -T {get_date} --variable {variable} -o {intermediatedir} -f {out_name} --force-download")   
          system(command, intern = TRUE)
          
          r=raster(glue("{intermediatedir}/{out_name}.nc"))
          r1=r^.25 ## barb's math
          r2 <- raster::resample(r1, template)  
          extent(r2)=extent(template)
          writeRaster(r2,glue("{finaldir}/l.chl.grd"),overwrite=T)
          
        } else{
          print(glue("Not grabbing CHL data. Most recent data is from {nearest_date}, which is lagged behind target date by {how_different} days")) 
        }
         
        }
      },
      error = function(e){
        message(glue("CMEMS chl-a not available {get_date}"))
        print(e)
      }
    )
        
        tryCatch(
          expr = {
        ## ROMS for humpback, anchovy, EPAC   
            if(!file.exists(glue("{finaldir}/sst.grd"))){
  handle_the_data(get_date = get_date, var="sst", template=template, save_var="sst",finaldir = finaldir)}
            
            if(!file.exists(glue("{finaldir}/bv.grd"))){
  handle_the_data(get_date = get_date, var="bbv_200", template=template, save_var="bv",finaldir = finaldir)}
            
            if(!file.exists(glue("{finaldir}/curl.grd"))){
  handle_the_data(get_date = get_date, var="curl", template=template, save_var="curl",finaldir = finaldir)}
            
            if(!file.exists(glue("{finaldir}/ild.grd"))){
  handle_the_data(get_date = get_date, var="ild_05", template=template, save_var="ild",finaldir = finaldir)}
            
            if(!file.exists(glue("{finaldir}/ssh.grd"))){
  handle_the_data(get_date = get_date, var="ssh", template=template, save_var="ssh",finaldir = finaldir)}
            
            if(!file.exists(glue("{finaldir}/su.grd"))){      
  handle_the_data(get_date = get_date, var="su", template=template, save_var="su",finaldir = finaldir)}
            
            if(!file.exists(glue("{finaldir}/sustr.grd"))){
  handle_the_data(get_date = get_date, var="sustr", template=template, save_var="sustr",finaldir = finaldir)}
            
            if(!file.exists(glue("{finaldir}/sv.grd"))){
  handle_the_data(get_date = get_date, var="sv", template=template, save_var="sv",finaldir = finaldir)}
            
            if(!file.exists(glue("{finaldir}/svstr.grd"))){
  handle_the_data(get_date = get_date, var="svstr", template=template, save_var="svstr",finaldir = finaldir)}
  
  eke=(raster(glue("{finaldir}/su.grd"))^2+raster(glue("{finaldir}/sv.grd"))^2)/2 %>%
    log();writeRaster(eke,glue("{finaldir}/EKE.grd"),overwrite=T)
  
      },
  error = function(e){
    message(glue("ROMS not available {get_date}"))
    print(e)
  }
    )
    
    },
    error = function(e){
      message(glue("ERDDAP sst not available {get_date}"))
      print(e)
    }
    )
    
   }
  }
}

library(tidyverse)
# date_range=seq(Sys.Date()-30,Sys.Date(),by=1) %>% as.character()

date_range=seq(as.Date("2023-03-01"),Sys.Date(),by=1) %>% as.character()
Get_Env_Data_B_batch(path=path,source_path=source_path,date_range=date_range)

