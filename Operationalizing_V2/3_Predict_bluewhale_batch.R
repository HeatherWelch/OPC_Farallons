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
source_path="/Users/EcoCast/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V2"


############# ----------------------------> End ################

predict_bluewhale <- function(path,source_path,date_range){
  
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
   studyarea=st_read(glue("{staticdir}/bluewhale/sa_square_coast3.shp"))
   studyarea <- sf:::st_zm(studyarea$geom)
   studyarea=as(studyarea, "Spatial")
   
   BRT_ws=readRDS(glue("{moddir}/blwh.res1.tc3.lr05.single.DecJun.final.rds"))
   BRT_sf=readRDS(glue("{moddir}/blwh.res1.tc3.lr05.single.JulNov.final.rds"))
   GAMM_ws=readRDS(glue("{moddir}/blwh.res1.gam.ws.mod1.rds"))
   GAMM_sf=readRDS(glue("{moddir}/blwh.res1.gam.sf.mod1.rds"))
  
  brt_static_covars=c("zsd_1.grd","z_.1.grd","slope_0.1.tif","aspect_0.1.tif")
  brt_static_stack=lapply(brt_static_covars,function(x)glue("{staticdir}/bluewhale/{x}")) %>% unlist() %>% stack()
  names(brt_static_stack)=c("zsd_1","z_0.1","slope","aspect")
  
  gam_static_covars=c("zsd_1.grd","z_.1.grd")
  gam_static_stack=lapply(gam_static_covars,function(x)glue("{staticdir}/bluewhale/{x}")) %>% unlist() %>% stack()
  names(gam_static_stack)=c("zsd_1","z_0.1")

  ############ 2. Define time and dynamic directories
  for(date in date_range){
    get_date=date
    print(get_date)
    
    year=year(get_date)
    month=month(get_date) %>% str_pad(.,2,side="left",pad=0)
    
  finaldir=glue("{envdir}/{get_date}") #change for each user
  
  print("**************************************************************************************")
  print(paste("Starting bluewhale prediction for ",get_date,". Time is ",Sys.time(),sep=""))
  
  if(!file.exists(glue("{rastersdir}/{year}/{month}/bluewhale_{get_date}.grd"))){
  
  tryCatch(
    expr = {
    
    ild=glue("{finaldir}/ild.grd") %>% 
      raster()
      names(ild)="ild_mean_0.1"
      
    ssh=glue("{finaldir}/ssh.grd") %>% 
      raster()
    names(ssh)="ssh_mean_0.1"
    
    sst=glue("{finaldir}/sst.grd") %>% 
      raster()
      names(sst)="sst_mean_0.1"
      
    curl=glue("{finaldir}/curl.grd") %>% 
        raster()
      names(curl)="curl_mean_0.5"
      
    sst_sd=glue("{finaldir}/sst_sd_.07.grd") %>% 
        raster()
      names(sst_sd)="sst_sd_1"
      
    ssh_sd=glue("{finaldir}/ssh_sd_.07.grd") %>% 
        raster()
      names(ssh_sd)="ssh_sd_1"
      
    eke=glue("{finaldir}/EKE.grd") %>% 
        raster()
      names(eke)="EKE_0.1"
      
    bv=glue("{finaldir}/bv.grd") %>% 
        raster()
      names(bv)="BV_frequency_mean_0.1"
      
    gam_stack=stack(c(sst,ssh_sd,ild,eke,gam_static_stack)) %>% 
      as.data.frame(stack,stringsAsFactors=F) %>% 
      mutate(ptt=723029)
    brt_stack=stack(c(curl,ild,ssh,sst,sst_sd,ssh_sd,eke,bv,brt_static_stack))%>% 
      as.data.frame(stack,stringsAsFactors=F) %>% 
      mutate(ptt=723029)
    
    #assign winter/spring model weightings for each week of year
    #weightings for summer/fall model are 1 - winter/spring weightings
    ws_weightings <- c(rep(1,22), .8,.6,.4,.2, rep(0,17),.2,.4,.6,.8, rep(1,6)) 
    
    ws_GAMM_pred <- predict.gam(GAMM_ws,newdata=gam_stack, type = 'response')
    ws_BRT_pred <- predict.gbm(BRT_ws,newdata=brt_stack,n.trees=1000,type='response')
    sf_GAMM_pred <- predict.gam(GAMM_sf,newdata=gam_stack, type = 'response')
    sf_BRT_pred <- predict.gbm(BRT_sf,newdata=brt_stack,n.trees=1000,type='response')
    
    ws_mod_preds <- cbind(ws_GAMM_pred, ws_BRT_pred); ws_mod_preds <- rowMeans(ws_mod_preds, na.rm=T)
    sf_mod_preds <- cbind(sf_GAMM_pred, sf_BRT_pred); sf_mod_preds <- rowMeans(sf_mod_preds, na.rm=T)
    
    # combine model predictions by weightings
    mod_preds <- (ws_mod_preds * ws_weightings[week(get_date)]) + (sf_mod_preds * (1-ws_weightings[week(get_date)]))
    
    ## make rasters 
    meanPredR <- setValues(template,mod_preds)%>%mask(.,studyarea)
    writeRaster(meanPredR,glue("{rastersdir}/{year}/{month}/bluewhale_{get_date}.grd"),overwrite=T)
    pred_df=rasterToPoints(meanPredR) %>% as.data.frame()
    
    pred_map=ggplot()+
      geom_tile(data=pred_df,aes(x = x, y = y, fill=layer))+
      scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
      geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
      theme_classic()+xlab(NULL)+ylab(NULL)+
      coord_sf(xlim = c(-134, -115.5), ylim = c(30,48),expand=F)+
      ggtitle(glue("Blue whale {get_date}"))+
      theme(legend.position = "bottom",
            legend.key.width = unit(1, 'cm'))
    
    png(glue("{mapssdir}/{year}/{month}/bluewhale_{get_date}.png"),width=8,height=11,units='cm',res=400,type = "cairo")
    par(ps=10)
    par(mar=c(0,0,0,0))
    par(cex=1)
    print({pred_map})
    # gg_hm
    dev.off()

    },
    error = function(e){
      message(glue("Some bluewhale environmental data was missing for {get_date}"))
      print(e)
    }
  )
    
  }

  print("**************************************************************************************")
  } 
}

library(tidyverse)
date_range=seq(as.Date("2023-03-01"),Sys.Date(),by=1) %>% as.character()

# date_range=seq(as.Date("2020-05-31"),as.Date("2020-06-02"),by=1) %>% as.character()
# date_range=seq(Sys.Date()-30,Sys.Date(),by=1) %>% as.character()

predict_bluewhale(path=path,source_path = source_path,date_range=date_range)

