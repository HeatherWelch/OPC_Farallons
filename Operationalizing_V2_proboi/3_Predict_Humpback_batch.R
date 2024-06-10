#######Get_Env_Data A - OPC
# Get data sequence number one : Create final and temporary envdirs, acquire all static variables
# ONLY RUN ONCE AT BEGINNING OF DAY
#adapted from EcoCast by Heather Welch (NOAA/UCSC) 

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
# path="/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization"
path="/Volumes/PROBOI/Heather_working/OPC"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
# source_path="/Users/heatherwelch/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V1"
source_path="/Users/EcoCast/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V2"


############# ----------------------------> End ################

predict_humpback <- function(path,source_path,date_range){
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)

  envdir=glue("{path}/daily_prediction_layers")
  outdir <- glue("{path}/end_products_final")
  staticdir=glue("{path}/static_variables")
  # temp=glue("{path}/raw_roms_data")
  moddir=glue("{path}/models")
  # intermediatedir=glue("{path}/intermediate")
  
  # flyersdir=glue("{outdir}/flyers")
   rastersdir=glue("{outdir}/rasters")
   mapssdir=glue("{outdir}/maps")
  # latestdir=glue("{outdir}/latest")
  # latestweekdir=glue("{outdir}/latest_week")
  # latestweeksmootheddir=glue("{outdir}/latest_week_smoothed")
  
  ############ 3. Define global objects
   template_humpback=read.csv(glue("{staticdir}/humpback/Grid_Nonrectangle_3km_WEAR_bathy.csv"))[, c("lon180", "lat")] %>% 
     rasterFromXYZ()
   load(glue("{moddir}/Mn3km_GAM.RData")) #Mn3km_GAM
  
  depth=read.csv(glue("{staticdir}/humpback/Grid_Nonrectangle_3km_WEAR_bathy.csv"))[, c("lon180", "lat","depth")] %>% 
    rasterFromXYZ()
  
  predict.gam.null.ck.log.off <- function(x,p.data,...)
  {
    if(length(names(x$coefficients)) == 1)
    {
      p.resp <- exp(as.numeric(x$coefficients[[1]]) + log(p.data$effort))
    } else {
      p.resp <- predict.gam(x, p.data,...)
    }
  }

  ############ 2. Define time and dynamic directories
  for(date in date_range){
    get_date=date
    print(get_date)
    
    
    year=year(get_date)
    month=month(get_date) %>% str_pad(.,2,side="left",pad=0)
    
  finaldir=glue("{envdir}/{get_date}") #change for each user
  
  print("**************************************************************************************")
  print(paste("Starting humpback prediction for ",get_date,". Time is ",Sys.time(),sep=""))
  
  if(!file.exists(glue("{rastersdir}/{year}/{month}/humpback_{get_date}.grd"))){
  
  tryCatch(
    expr = {
    
    ild=glue("{finaldir}/ild_humpback.grd") %>% 
      raster()
      names(ild)="MLD"
      
    ssh=glue("{finaldir}/ssh_humpback.grd") %>% 
      raster()
    names(ssh)="SSH"
    
    sst=glue("{finaldir}/sst_humpback.grd") %>% 
      raster()
      names(sst)="mSST"
    
    newpreds=stack(ild,ssh,sst,depth) %>% 
      rasterToPoints() %>% as.data.frame() %>% 
      mutate(effort=1) %>% 
      mutate(preds=predict.gam.null.ck.log.off(Mn3km_GAM,., type = "response"))
    
    pred_ras=rasterFromXYZ(newpreds %>% dplyr::select(x,y,preds))
    writeRaster(pred_ras,glue("{rastersdir}/{year}/{month}/humpback_{get_date}.grd"),overwrite=T)
    
    pred_map=ggplot()+
      geom_tile(data=newpreds,aes(x = x, y = y, fill=preds))+
      scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
      geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
      theme_classic()+xlab(NULL)+ylab(NULL)+
      coord_sf(xlim = c(-127, -115.5), ylim = c(30,46),expand=F)+
      ggtitle(glue("Humpback {get_date}"))+
      theme(legend.position = "bottom",
            legend.key.width = unit(1, 'cm'))
    
    png(glue("{mapssdir}/{year}/{month}/humpback_{get_date}.png"),width=8,height=11,units='cm',res=400,type = "cairo")
    par(ps=10)
    par(mar=c(0,0,0,0))
    par(cex=1)
    print({pred_map})
    # gg_hm
    dev.off()

    },
    error = function(e){
      message(glue("Some humback environmental data was missing for {get_date}"))
      print(e)
    }
  )
    
  }

  print("**************************************************************************************")
  } 
}

library(tidyverse)
date_range=seq(as.Date("2011-01-01"),Sys.Date(),by=1) %>% as.character()

# date_range=seq(as.Date("2020-05-31"),as.Date("2020-06-02"),by=1) %>% as.character()
# date_range=seq(Sys.Date()-30,Sys.Date(),by=1) %>% as.character()

predict_humpback(path=path,source_path = source_path,date_range=date_range)

