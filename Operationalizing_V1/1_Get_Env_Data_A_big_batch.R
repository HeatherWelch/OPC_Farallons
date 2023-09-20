#######Get_Env_Data A - OPC
# Get data sequence number one : Create final and temporary envdirs, acquire all static variables
# ONLY RUN ONCE AT BEGINNING OF DAY
#adapted from EcoCast by Heather Welch (NOAA/UCSC) 

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V1"
############# ----------------------------> End ################

Get_Env_Data_A <- function(path,source_path,date_range){
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)

  envdir=glue("{path}/daily_prediction_layers")
  outdir <- glue("{path}/end_products")
  staticdir=glue("{path}/static_variables")
  # temp=glue("{path}/raw_roms_data")
  moddir=glue("{path}/models")
  # intermediatedir=glue("{path}/intermediate")
  
  # flyersdir=glue("{outdir}/flyers")
  # rastersdir=glue("{outdir}/rasters")
  # mapssdir=glue("{outdir}/maps")
  # latestdir=glue("{outdir}/latest")
  # latestweekdir=glue("{outdir}/latest_week")
  # latestweeksmootheddir=glue("{outdir}/latest_week_smoothed")
  
  ############ 2. Define time and dynamic directories
  for(date in date_range){
    get_date=date
    print(get_date)
    
  
  finaldir=glue("{envdir}/{get_date}") #change for each user
  if(!file.exists(finaldir)){
    dir.create(finaldir)
  }
  
  print("**************************************************************************************")
  print(paste("Starting Benioff run for ",get_date,". Time is ",Sys.time(),sep=""))
  
  ############ 3. Define global objects
  template=raster(glue("{path}/static_variables/template.grd"))
  
  ################### Acquire static variables
  print("Starting script Env_Data_A.R")
  
  ############ 5. lunillum
  print(paste("Calculating lunillum for ",get_date,sep=""))
  value <- lunar.illumination(as.Date(get_date))
  lunar_ras=template
  values(lunar_ras)=value
  writeRaster(lunar_ras,paste(finaldir,"/lunillum",sep=""),overwrite=T)
  
  ############ 6. wipe files in /latest
  # mean=list.files(paste0(benioffdir,"latest/"),full.names = T)
  # lapply(mean,function(x)file.remove(x))
  
  # se=list.files(paste0(benioffdir,"brt/se/latest/"),full.names = T)
  # lapply(se,function(x)file.remove(x))
  
  #warnings()
  print("**************************************************************************************")
  } 
}
# date_range=seq(Sys.Date()-30,Sys.Date(),by=1) %>% as.character()

date_range=seq(Sys.Date()-100,Sys.Date()-79,by=1) %>% as.character()

Get_Env_Data_A(path=path,source_path = source_path,date_range=date_range)

