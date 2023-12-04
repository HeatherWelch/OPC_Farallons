 #######Get_Env_Data A - OPC
# Get data sequence number one : Create final and temporary envdirs, acquire all static variables
# ONLY RUN ONCE AT BEGINNING OF DAY
#adapted from EcoCast by Heather Welch (NOAA/UCSC) 

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
# path="/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization"
path="/Users/EcoCast/Dropbox/OPC_Farallons/operationalization"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
# source_path="/Users/heatherwelch/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V1"
source_path="/Users/EcoCast/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V1"


############# ----------------------------> End ################

predict_anchovy <- function(path,source_path,date_range){
  
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
  template=raster(glue("{path}/static_variables/template.grd"))
  load(glue("{moddir}/anchovyGAM_updatedMay2023.rda")) #anchGAM
  
  distland=readRDS(glue("{staticdir}/anchovy/distLandROMSpoints.rds")) %>% 
    dplyr::select(lon,lat,distLand) %>% 
    rasterFromXYZ()
  distland2 <- raster::resample(distland, template)  
  extent(distland2)=extent(template)
  crs(distland2)=crs(template)
  names(distland2)="distLand"
  
  ############ 2. Define time and dynamic directories
  for(date in date_range){
    get_date=date
    print(get_date)
    
    year=year(get_date)
    month=month(get_date) %>% str_pad(.,2,side="left",pad=0)
    
  finaldir=glue("{envdir}/{get_date}") #change for each user
  
  print("**************************************************************************************")
  print(paste("Starting Anchovy prediction for ",get_date,". Time is ",Sys.time(),sep=""))
  
  if(!file.exists(glue("{rastersdir}/{year}/{month}/anchovy_{get_date}.grd"))){
  
  tryCatch(
    expr = {
    bv=glue("{finaldir}/bv.grd") %>% 
      raster()
    names(bv)="BV"
    
    ild=glue("{finaldir}/ild.grd") %>% 
      raster()
      names(ild)="ild"
      
    ssh=glue("{finaldir}/ssh.grd") %>% 
      raster()
    names(ssh)="ssh"
    
    sst=glue("{finaldir}/sst.grd") %>% 
      raster()
      names(sst)="sst"
    
    logEKE=glue("{finaldir}/EKE.grd") %>% 
      raster()
    names(logEKE)="logEKE"
    
    sst_sd=glue("{finaldir}/sst_sd_.07.grd") %>% 
      raster()
    names(sst_sd)="sst_sd"
    
    ssh_sd=glue("{finaldir}/ssh_sd_.07.grd") %>% 
      raster()
    names(ssh_sd)="ssh_sd"
    
    chl4th=glue("{finaldir}/l.chl.grd") %>% 
      raster()
    names(chl4th)="chl4th"
    
    lunar=glue("{finaldir}/lunillum.grd") %>% 
      raster()
    names(lunar)="lunar"
    
    newpreds=stack(bv,ild,ssh,sst,logEKE,sst_sd,ssh_sd,chl4th,distland2,lunar) %>% 
      rasterToPoints() %>% as.data.frame() %>% 
      mutate(ssb=3548420) %>% 
      mutate(preds=predict(anchGAM,., type = "response"))
    
    pred_ras=rasterFromXYZ(newpreds %>% dplyr::select(x,y,preds))
    writeRaster(pred_ras,glue("{rastersdir}/{year}/{month}/anchovy_{get_date}.grd"),overwrite=T)
    
    pred_map=ggplot()+
      geom_tile(data=newpreds,aes(x = x, y = y, fill=preds))+
      scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
      geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
      theme_classic()+xlab(NULL)+ylab(NULL)+
      coord_sf(xlim = c(-134, -115.5), ylim = c(30,46),expand=F)+
      ggtitle(glue("Anchovy {get_date}"))+
      theme(legend.position = "bottom",
            legend.key.width = unit(1, 'cm'))
    
    png(glue("{mapssdir}/{year}/{month}/anchovy_{get_date}.png"),width=11,height=11,units='cm',res=400,type = "cairo")
    par(ps=10)
    par(mar=c(0,0,0,0))
    par(cex=1)
    print({pred_map})
    # gg_hm
    dev.off()

    },
    error = function(e){
      message(glue("Some anchovy environmental data was missing for {get_date}"))
      print(e)
    }
  )
    
  }

  print("**************************************************************************************")
  } 
}

library(tidyverse)

date_range=seq(as.Date("2023-03-01"),Sys.Date(),by=1) %>% as.character()

# date_range=seq(as.Date("2023-08-19"),as.Date("2023-08-21"),by=1) %>% as.character()
# date_range=seq(Sys.Date()-30,Sys.Date(),by=1) %>% as.character()

predict_anchovy(path=path,source_path = source_path,date_range=date_range)

