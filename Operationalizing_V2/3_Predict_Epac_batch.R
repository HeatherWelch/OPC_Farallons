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

predict_epac <- function(path,source_path,date_range){
  
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
  load(glue("{moddir}/combined_models.Rdata")) #combined_models
  
  z=raster(glue("{staticdir}/epac/z.grd"))
  z<-replace(z, z> 0, NA)
  z=log10(abs(z))
  names(z)="z"
  
  z_sd=raster(glue("{staticdir}/epac/z_sd.grd"))
  names(z_sd)="z_sd"
  
  distance_to_shore=raster(glue("{staticdir}/epac/Distance_from_shore.grd"))
  names(distance_to_shore)="Distance_from_shore"
  distance_to_shore<- replace(distance_to_shore, distance_to_shore> 150000, NA)
  
  pred_domain=mask(distance_to_shore,z)

  ############ 2. Define time and dynamic directories
  for(date in date_range){
    get_date=date
    get_date_month=month(as.Date(get_date))
    start_date=as.Date(get_date)-30
    start_date_month=month(as.Date(start_date))
    print(get_date)
    
    year=year(get_date)
    month=month(get_date) %>% str_pad(.,2,side="left",pad=0)
    
    ### can only predict april - august
    okay_start_months=c(3,4,5,6,7,8) ## okay for it to do e.g. april 1st
    okay_end_months=c(4,5,6,7,8) ## will stop Aug 31st
    
    if(start_date_month %in% okay_start_months & get_date_month %in% okay_end_months){
      print("Get_date is between April and May, making prediction")
      print(glue("start date month is {start_date_month}; end date month is {get_date_month}"))
    
  finaldir=glue("{envdir}/{get_date}") #change for each user
  
  if(file.exists(glue("{outdir}/epac_metadata/{year}/{month}/metadata_{get_date}.csv"))){
  metadata=read.csv(glue("{outdir}/epac_metadata/{year}/{month}/metadata_{get_date}.csv")) %>% 
    pull(n_days_w_data) %>% mean()
  }else{metadata=0}
  
  print("**************************************************************************************")
  print(paste("Starting Epac prediction for ",get_date,". Time is ",Sys.time(),sep=""))
  
  if(!file.exists(glue("{rastersdir}/{year}/{month}/epac_{get_date}.grd")) || (metadata!=31 & start_date_month>3)){
    if(!file.exists(glue("{rastersdir}/{year}/{month}/epac_{get_date}.grd"))){print("File doesn't exist")}
    else if(metadata!=31& start_date_month>3){print("Prediction being remade - didn't have full month available")
      file.remove(read.csv(glue("{outdir}/epac_metadata/{year}/{month}/metadata_{get_date}.csv"))) ## delete so git will register new file
      file.remove(glue("{rastersdir}/{year}/{month}/epac_{get_date}.grd"))
      }
  tryCatch(
    expr = {
      
      one_month=seq(start_date,as.Date(get_date),by=1)
      
      folders=list.files(envdir,full.names = T) %>% 
        grep(paste(one_month,collapse="|"),.,value=T)
      
    bv=list.files(folders,recursive = T,full.names = T) %>% 
      grep("bv.grd",.,value = T) %>% stack()
    bv_length=nlayers(bv)
    bv_ras=bv %>% mean()
    names(bv_ras)="bv"
      
      ild=list.files(folders,recursive = T,full.names = T) %>% 
        grep("ild.grd",.,value = T) %>% stack()
      ild_length=nlayers(ild)
      ild_ras=ild %>% mean()
      names(ild_ras)="ild"
    
    ssh=list.files(folders,recursive = T,full.names = T) %>% 
      grep("ssh.grd",.,value = T) %>% stack()
    ssh_length=nlayers(ssh)
    ssh_ras=ssh %>% mean()
    names(ssh_ras)="ssh"
      
      sst=list.files(folders,recursive = T,full.names = T) %>% 
        grep("sst.grd",.,value = T) %>% stack()
      sst_length=nlayers(sst)
      sst_ras=sst %>% mean()
      names(sst_ras)="sst"
    
    su=list.files(folders,recursive = T,full.names = T) %>% 
      grep("su.grd",.,value = T) %>% stack()
    su_length=nlayers(su)
    su_ras=su %>% mean()
    names(su_ras)="su"
    
    sv=list.files(folders,recursive = T,full.names = T) %>% 
      grep("sv.grd",.,value = T) %>% stack()
    sv_length=nlayers(sv)
    sv_ras=sv %>% mean()
    names(sv_ras)="sv"
    
    sustr=list.files(folders,recursive = T,full.names = T) %>% 
      grep("sustr.grd",.,value = T) %>% stack()
    sustr_length=nlayers(sustr)
    sustr_ras=sustr %>% mean()
    names(sustr_ras)="sustr"
    
    svstr=list.files(folders,recursive = T,full.names = T) %>% 
      grep("svstr.grd",.,value = T) %>% stack()
    svstr_length=nlayers(svstr)
    svstr_ras=svstr %>% mean()
    names(svstr_ras)="svstr"
    
    sst_sd=list.files(folders,recursive = T,full.names = T) %>% 
      grep("sst_sd_.03.grd",.,value = T) %>% stack()
    sst_sd_length=nlayers(sst_sd)
    sst_sd_ras=sst_sd %>% mean()
    names(sst_sd_ras)="sst_sd_0.3"

    ssh_sd=list.files(folders,recursive = T,full.names = T) %>% 
      grep("ssh_sd_.03.grd",.,value = T) %>% stack()
    ssh_sd_length=nlayers(ssh_sd)
    ssh_sd_ras=ssh_sd %>% mean()
    names(ssh_sd_ras)="ssh_sd_0.3"
    
    curl=list.files(folders,recursive = T,full.names = T) %>% 
      grep("curl.grd",.,value = T) %>% stack()
    curl_length=nlayers(curl)
    curl_ras=curl %>% mean()
    names(curl_ras)="curl"
    
    metadata=data.frame(
      var=c("bv","ild","ssh","sst","su","sv","sustr","svstr","sst_sd","ssh_sd","curl"),
      prediction_date=get_date,
      average_start=start_date,
      average_end=get_date,
      # days_in_average=as.character(difftime(as.Date(get_date),start_date)),
      n_days_w_data=c(bv_length,ild_length,ssh_length,sst_length,su_length,sv_length,sustr_length,svstr_length,sst_sd_length,ssh_sd_length,curl_length)
    )
    
    write.csv(metadata,glue("{outdir}/epac_metadata/{year}/{month}/metadata_{get_date}.csv"))
    
    newpreds=stack(bv_ras,ild_ras,ssh_ras,sst_ras,su_ras,sv_ras,sustr_ras,
                   svstr_ras,sst_sd_ras,ssh_sd_ras,curl_ras,z,z_sd,distance_to_shore) %>% 
      rasterToPoints() %>% as.data.frame() %>% 
      mutate(preds1=predict(combined_models[[1]][[2]],., type = "response",n.trees=combined_models[[1]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds2=predict(combined_models[[2]][[2]],., type = "response",n.trees=combined_models[[2]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds3=predict(combined_models[[3]][[2]],., type = "response",n.trees=combined_models[[3]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds4=predict(combined_models[[4]][[2]],., type = "response",n.trees=combined_models[[4]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds5=predict(combined_models[[5]][[2]],., type = "response",n.trees=combined_models[[5]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds6=predict(combined_models[[6]][[2]],., type = "response",n.trees=combined_models[[6]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds7=predict(combined_models[[7]][[2]],., type = "response",n.trees=combined_models[[7]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds8=predict(combined_models[[8]][[2]],., type = "response",n.trees=combined_models[[8]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds9=predict(combined_models[[9]][[2]],., type = "response",n.trees=combined_models[[9]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(preds10=predict(combined_models[[10]][[2]],., type = "response",n.trees=combined_models[[10]][[2]]$gbm.call$best.trees, family="gaussian")) %>% 
      mutate(pred_mean = rowMeans(dplyr::select(., starts_with("preds")), na.rm = TRUE)) %>% 
      mutate(pred_mean2=case_when(pred_mean<0~0,## replace less than 0 w 0
                                 T~pred_mean))
    
    pred_ras=rasterFromXYZ(newpreds %>% dplyr::select(x,y,pred_mean2)) 
    pred_ras_clip=mask(pred_ras,pred_domain)
    
    writeRaster(pred_ras_clip,glue("{rastersdir}/{year}/{month}/epac_{get_date}.grd"),overwrite=T)
    
    df_plot=rasterToPoints(pred_ras_clip) %>% 
      as.data.frame()
    pred_map=ggplot()+
      geom_tile(data=df_plot,aes(x = x, y = y, fill=pred_mean2))+
      scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
      geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
      theme_classic()+xlab(NULL)+ylab(NULL)+
      coord_sf(xlim = c(-127, -115.5), ylim = c(30,46),expand=F)+
      ggtitle(glue("EPAC {get_date}"))+
      theme(legend.position = "bottom",
            legend.key.width = unit(1, 'cm'))
    
    png(glue("{mapssdir}/{year}/{month}/epac_{get_date}.png"),width=8,height=11,units='cm',res=400,type = "cairo")
    par(ps=10)
    par(mar=c(0,0,0,0))
    par(cex=1)
    print({pred_map})
    # gg_hm
    dev.off()

    },
    error = function(e){
      message(glue("Some epac environmental data was missing for {get_date}"))
      print(e)
    }
  )
}
  print("**************************************************************************************")
  }
  } 
}

library(tidyverse)

date_range=seq(as.Date("2023-03-01"),Sys.Date(),by=1) %>% as.character()

 # date_range=seq(as.Date("2023-08-19"),as.Date("2023-08-21"),by=1) %>% as.character()
# date_range=seq(Sys.Date()-30,Sys.Date(),by=1) %>% as.character()

predict_epac(path=path,source_path = source_path,date_range=date_range)

 