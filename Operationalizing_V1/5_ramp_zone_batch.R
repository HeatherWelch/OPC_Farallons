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

ramp_zone <- function(path,source_path,date_range){
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)

  envdir=glue("{path}/daily_prediction_layers")
  outdir <- glue("{path}/end_products")
  staticdir=glue("{path}/static_variables")
  # temp=glue("{path}/raw_roms_data")
  moddir=glue("{path}/models")
  intermediatedir=glue("{path}/intermediate")
  
  # flyersdir=glue("{outdir}/flyers")
   rastersdir=glue("{outdir}/rasters")
   mapssdir=glue("{outdir}/maps")
  # latestdir=glue("{outdir}/latest")
  # latestweekdir=glue("{outdir}/latest_week")
  # latestweeksmootheddir=glue("{outdir}/latest_week_smoothed")
  
  ############ 3. Define global objects
   rz3=st_read(glue("{staticdir}/ramp_zones/MAN_CA_Fishing_Zone3.shp"))
   rz3 <- sf:::st_zm(rz3$geom)
   rz3=as(rz3, "Spatial")
   rz3 <-spTransform(rz3, CRS("+proj=longlat +datum=WGS84")) #convert from UTM to LatLon
   rz3 = as(rz3, 'SpatialPolygons')

   rz4=st_read(glue("{staticdir}/ramp_zones/MAN_CA_Fishing_Zone4.shp"))
   rz4 <- sf:::st_zm(rz4$geom)
   rz4=as(rz4, "Spatial")
   rz4 <-spTransform(rz4, CRS("+proj=longlat +datum=WGS84")) #convert from UTM to LatLon
   rz4 = as(rz4, 'SpatialPolygons')
   
   ras=list.files(glue("{outdir}/rasters"),pattern=".grd",full.names = T) %>% 
     grep(paste(date_range,collapse = "|"),.,value=T)
   
   hump_dates=ras %>%
     grep("humpback",.,value=T) %>%
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/humpback_","",.)%>%
     gsub(".grd","",.)
   anch_dates=ras %>%
     grep("anchovy",.,value=T) %>%
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/anchovy_","",.)%>%
     gsub(".grd","",.)
   epac_dates=ras %>%
     grep("epac",.,value=T) %>%
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/epac_","",.)%>%
     gsub(".grd","",.)
   bluewhale_dates=ras %>%
     grep("bluewhale",.,value=T) %>%
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/bluewhale_","",.)%>%
     gsub(".grd","",.)

   # initial indicator creation only ####
   ## file creation dates
   # creation_times=ras %>% 
   #   lapply(.,function(x)as.character(as.Date(file.mtime(x))))%>% unlist()
   # 
   # raster_get_dates=ras %>%
   #   gsub(glue("{outdir}/rasters"),"",.) %>%
   #   gsub("/humpback_","",.)%>%
   #   gsub("/anchovy_","",.)%>%
   #   gsub("/epac_","",.)%>%
   #   gsub("/bluewhale_","",.)%>%
   #   gsub(".grd","",.)
   # 
   # species=ras %>%
   #   gsub(glue("{outdir}/rasters/"),"",.) %>%
   #   gsub(".grd","",.) %>% 
   #   substr(.,1,nchar(.)-11)
   # 
   # indicator_metadata=data.frame(
   #   What=species,
   #   get_date=raster_get_dates,
   #   creation_time=creation_times
   # )
   # 
   ## for testing file update code
   # indicator_metadata[236:240,3]="2013-11-11"
   # write_rds(indicator_metadata,glue("{intermediatedir}/indication_creationTime_metadata/indicator_metadata.rds"))
   
   ## humpback #### 
   # hump=ras %>% 
   #   grep("humpback",.,value=T) %>% stack()
   # 
   # hump_dates=ras %>% 
   #   grep("humpback",.,value=T) %>% 
   #   gsub(glue("{outdir}/rasters"),"",.) %>% 
   #   gsub("/humpback_","",.)%>% 
   #   gsub(".grd","",.)
   # 
   #  hump_r3=raster::extract(hump,rz3,fun=base::mean,df=T,na.rm=T) %>% 
   #   dplyr::select(-ID) %>% 
   #   gather(Date,value)%>% 
   #   mutate(zone="RAMP zone 3") %>% 
   #   mutate(Date=hump_dates)
   # 
   # hump_r4=raster::extract(hump,rz4,fun=base::mean,df=T,na.rm=T) %>% 
   #   dplyr::select(-ID) %>% 
   #   gather(Date,value)%>% 
   #   mutate(zone="RAMP zone 4") %>% 
   #   mutate(Date=hump_dates)
   # 
   # hump_master=rbind(hump_r3,hump_r4) %>% 
   #   mutate(species="Humpback") %>% 
   #   mutate(metric="Density")%>% 
   #   mutate(creation_date=as.Date(Sys.Date()))
   # 
   # write.csv(hump_master,glue("{outdir}/ramp_zone_indicators/humpback_ramp_zone34_indicator.csv"))
   
   ## anchovy #### 
   # anch=ras %>% 
   #   grep("anchovy",.,value=T) %>% stack()
   # 
   # anch_dates=ras %>% 
   #   grep("anchovy",.,value=T) %>% 
   #   gsub(glue("{outdir}/rasters"),"",.) %>% 
   #   gsub("/anchovy_","",.)%>% 
   #   gsub(".grd","",.)
   # 
   # anch_r3=raster::extract(anch,rz3,fun=base::mean,df=T,na.rm=T) %>% 
   #   dplyr::select(-ID) %>% 
   #   gather(Date,value)%>% 
   #   mutate(zone="RAMP zone 3") %>% 
   #   mutate(Date=anch_dates)
   # 
   # anch_r4=raster::extract(anch,rz4,fun=base::mean,df=T,na.rm=T) %>% 
   #   dplyr::select(-ID) %>% 
   #   gather(Date,value)%>% 
   #   mutate(zone="RAMP zone 4") %>% 
   #   mutate(Date=anch_dates)
   # 
   # anch_master=rbind(anch_r3,anch_r4) %>% 
   #   mutate(species="anchovy") %>% 
   #   mutate(metric="Probability of presence")%>% 
   #   mutate(creation_date=as.Date(Sys.Date()))
   # 
   # write.csv(anch_master,glue("{outdir}/ramp_zone_indicators/anchovy_ramp_zone34_indicator.csv"))
   
   ## epac #### 
   # epac=ras %>% 
   #   grep("epac",.,value=T) %>% stack()
   # 
   # epac_dates=ras %>% 
   #   grep("epac",.,value=T) %>% 
   #   gsub(glue("{outdir}/rasters"),"",.) %>% 
   #   gsub("/epac_","",.)%>% 
   #   gsub(".grd","",.)
   # 
   # epac_r3=raster::extract(epac,rz3,fun=base::mean,df=T,na.rm=T) %>% 
   #   dplyr::select(-ID) %>% 
   #   gather(Date,value)%>% 
   #   mutate(zone="RAMP zone 3") %>% 
   #   mutate(Date=epac_dates)
   # 
   # epac_r4=raster::extract(epac,rz4,fun=base::mean,df=T,na.rm=T) %>% 
   #   dplyr::select(-ID) %>% 
   #   gather(Date,value)%>% 
   #   mutate(zone="RAMP zone 4") %>% 
   #   mutate(Date=epac_dates)
   # 
   # epac_master=rbind(epac_r3,epac_r4) %>% 
   #   mutate(species="epac") %>% 
   #   mutate(metric="CPUE")%>% 
   #   mutate(creation_date=as.Date(Sys.Date()))
   # 
   # write.csv(epac_master,glue("{outdir}/ramp_zone_indicators/epac_ramp_zone34_indicator.csv"))
   
   ## bluewhale #### 
   # bluewhale=ras %>% 
   #   grep("bluewhale",.,value=T) %>% stack()
   # 
   # bluewhale_dates=ras %>% 
   #   grep("bluewhale",.,value=T) %>% 
   #   gsub(glue("{outdir}/rasters"),"",.) %>% 
   #   gsub("/bluewhale_","",.)%>% 
   #   gsub(".grd","",.)
   # 
   # bluewhale_r3=raster::extract(bluewhale,rz3,fun=base::mean,df=T,na.rm=T) %>% 
   #   dplyr::select(-ID) %>% 
   #   gather(Date,value)%>% 
   #   mutate(zone="RAMP zone 3") %>% 
   #   mutate(Date=bluewhale_dates)
   # 
   # bluewhale_r4=raster::extract(bluewhale,rz4,fun=base::mean,df=T,na.rm=T) %>% 
   #   dplyr::select(-ID) %>% 
   #   gather(Date,value)%>% 
   #   mutate(zone="RAMP zone 4") %>% 
   #   mutate(Date=bluewhale_dates)
   # 
   # bluewhale_master=rbind(bluewhale_r3,bluewhale_r4) %>% 
   #   mutate(species="bluewhale") %>% 
   #   mutate(metric="Probability of presence") %>% 
   #   mutate(creation_date=as.Date(Sys.Date()))
   # 
   # write.csv(bluewhale_master,glue("{outdir}/ramp_zone_indicators/bluewhale_ramp_zone34_indicator.csv"))
   
   
   #1. read in indicators ####
   hump_ind=read.csv(glue("{outdir}/ramp_zone_indicators/humpback_ramp_zone34_indicator.csv")) %>% 
     dplyr::select(-c(X))
   anch_ind=read.csv(glue("{outdir}/ramp_zone_indicators/anchovy_ramp_zone34_indicator.csv"))%>% 
     dplyr::select(-c(X))
   epac_ind=read.csv(glue("{outdir}/ramp_zone_indicators/epac_ramp_zone34_indicator.csv"))%>% 
     dplyr::select(-c(X))
   blue_ind=read.csv(glue("{outdir}/ramp_zone_indicators/bluewhale_ramp_zone34_indicator.csv"))%>% 
     dplyr::select(-c(X))
   
   # what's missing from the indicators ####
   ## 2. what's been updated since the day before in EPAC ####
   # file creation dates
   old_metadata=read_rds(glue("{intermediatedir}/indication_creationTime_metadata/indicator_metadata.rds")) %>% 
     filter(What=="epac")
   
   creation_times2=ras %>%
     lapply(.,function(x)as.character(as.Date(file.mtime(x))))%>% unlist()
   
   raster_get_dates=ras %>%
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/humpback_","",.)%>%
     gsub("/anchovy_","",.)%>%
     gsub("/epac_","",.)%>%
     gsub("/bluewhale_","",.)%>%
     gsub(".grd","",.)
   
   species=ras %>%
     gsub(glue("{outdir}/rasters/"),"",.) %>%
     gsub(".grd","",.) %>%
     substr(.,1,nchar(.)-11)
   
   new_metadata=data.frame(
     What=species,
     get_date=raster_get_dates,
     new_creation_time=creation_times2
   )
   
   metadata_diff=left_join(old_metadata,new_metadata) %>% 
     mutate(difference=as.Date(new_creation_time)-as.Date(creation_time)) %>% 
     mutate(difference=as.numeric(difference)) %>% 
     filter(difference!=0) %>% 
     pull(get_date)
   
   ## write it out so we can compare against it the next time
   indicator_metadata=new_metadata %>% 
     rename(creation_time=new_creation_time)
    write_rds(indicator_metadata,glue("{intermediatedir}/indication_creationTime_metadata/indicator_metadata.rds"))
  
    ## remove these out of date days from the epac indicator
    epac_ind=epac_ind %>% 
      filter(!Date %in% metadata_diff)
   
   ## 3. what's missing in each indicator ####
   hump_dates_missing=hump_ind %>% pull(Date) %>% 
     setdiff(hump_dates,.)
   anch_dates_missing=anch_ind %>% pull(Date) %>% 
     setdiff(anch_dates,.)
   epac_dates_missing=epac_ind %>% pull(Date) %>% 
     setdiff(epac_dates,.)
   epac_dates_missing=c(epac_dates_missing,metadata_diff) ## adding in dates that have been updated for epac
   blue_dates_missing=blue_ind %>% pull(Date) %>% 
     setdiff(bluewhale_dates,.)
   
   # adding missing days to the indicators ####
   ## humpback #### 
   if(length(hump_dates_missing)>0){
   hump=ras %>%
     grep("humpback",.,value=T) %>%
     grep(paste(epac_dates_missing,collapse = "|"),.,value = T) %>% 
     stack()

   hump_dates=ras %>%
     grep("humpback",.,value=T) %>%
     grep(paste(epac_dates_missing,collapse = "|"),.,value = T) %>% 
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/humpback_","",.)%>%
     gsub(".grd","",.)

    hump_r3=raster::extract(hump,rz3,fun=base::mean,df=T,na.rm=T) %>%
     dplyr::select(-ID) %>%
     gather(Date,value)%>%
     mutate(zone="RAMP zone 3") %>%
     mutate(Date=hump_dates)

   hump_r4=raster::extract(hump,rz4,fun=base::mean,df=T,na.rm=T) %>%
     dplyr::select(-ID) %>%
     gather(Date,value)%>%
     mutate(zone="RAMP zone 4") %>%
     mutate(Date=hump_dates)

   hump_master=rbind(hump_r3,hump_r4) %>%
     mutate(species="Humpback") %>%
     mutate(metric="Density")%>%
     mutate(creation_date=as.Date(Sys.Date()))%>% 
     rbind(.,hump_ind) %>% 
     arrange(Date)

   write.csv(hump_master,glue("{outdir}/ramp_zone_indicators/humpback_ramp_zone34_indicator.csv"))
   }
   
   ## anchovy #### 
   if(length(anch_dates_missing)>0){
   anch=ras %>%
     grep("anchovy",.,value=T) %>%
     grep(paste(anch_dates_missing,collapse = "|"),.,value = T) %>% 
     stack()

   anch_dates=ras %>%
     grep("anchovy",.,value=T) %>%
     grep(paste(anch_dates_missing,collapse = "|"),.,value = T) %>% 
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/anchovy_","",.)%>%
     gsub(".grd","",.)

   anch_r3=raster::extract(anch,rz3,fun=base::mean,df=T,na.rm=T) %>%
     dplyr::select(-ID) %>%
     gather(Date,value)%>%
     mutate(zone="RAMP zone 3") %>%
     mutate(Date=anch_dates)

   anch_r4=raster::extract(anch,rz4,fun=base::mean,df=T,na.rm=T) %>%
     dplyr::select(-ID) %>%
     gather(Date,value)%>%
     mutate(zone="RAMP zone 4") %>%
     mutate(Date=anch_dates)

   anch_master=rbind(anch_r3,anch_r4) %>%
     mutate(species="anchovy") %>%
     mutate(metric="Probability of presence")%>%
     mutate(creation_date=as.Date(Sys.Date()))%>% 
     rbind(.,anch_ind) %>% 
     arrange(Date)

   write.csv(anch_master,glue("{outdir}/ramp_zone_indicators/anchovy_ramp_zone34_indicator.csv"))
   }
   
   ## epac #### 
   if(length(epac_dates_missing)>0){
   epac=ras %>%
     grep("epac",.,value=T) %>% 
     grep(paste(epac_dates_missing,collapse = "|"),.,value = T) %>% 
     stack()

   epac_dates=ras %>%
     grep("epac",.,value=T) %>%
     grep(paste(epac_dates_missing,collapse = "|"),.,value = T) %>% 
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/epac_","",.)%>%
     gsub(".grd","",.)

   epac_r3=raster::extract(epac,rz3,fun=base::mean,df=T,na.rm=T) %>%
     dplyr::select(-ID) %>%
     gather(Date,value)%>%
     mutate(zone="RAMP zone 3") %>%
     mutate(Date=epac_dates)

   epac_r4=raster::extract(epac,rz4,fun=base::mean,df=T,na.rm=T) %>%
     dplyr::select(-ID) %>%
     gather(Date,value)%>%
     mutate(zone="RAMP zone 4") %>%
     mutate(Date=epac_dates)

   epac_master=rbind(epac_r3,epac_r4) %>%
     mutate(species="epac") %>%
     mutate(metric="CPUE")%>%
     mutate(creation_date=as.Date(Sys.Date())) %>% 
     rbind(.,epac_ind) %>% 
     arrange(Date)

   write.csv(epac_master,glue("{outdir}/ramp_zone_indicators/epac_ramp_zone34_indicator.csv"))
   }
   
   ## bluewhale #### 
   if(length(blue_dates_missing)>0){
   bluewhale=ras %>%
     grep("bluewhale",.,value=T) %>%
     grep(paste(blue_dates_missing,collapse = "|"),.,value = T) %>% 
     stack()

   bluewhale_dates=ras %>%
     grep("bluewhale",.,value=T) %>%
     grep(paste(blue_dates_missing,collapse = "|"),.,value = T) %>% 
     gsub(glue("{outdir}/rasters"),"",.) %>%
     gsub("/bluewhale_","",.)%>%
     gsub(".grd","",.)

   bluewhale_r3=raster::extract(bluewhale,rz3,fun=base::mean,df=T,na.rm=T) %>%
     dplyr::select(-ID) %>%
     gather(Date,value)%>%
     mutate(zone="RAMP zone 3") %>%
     mutate(Date=bluewhale_dates)

   bluewhale_r4=raster::extract(bluewhale,rz4,fun=base::mean,df=T,na.rm=T) %>%
     dplyr::select(-ID) %>%
     gather(Date,value)%>%
     mutate(zone="RAMP zone 4") %>%
     mutate(Date=bluewhale_dates)

   bluewhale_master=rbind(bluewhale_r3,bluewhale_r4) %>%
     mutate(species="bluewhale") %>%
     mutate(metric="Probability of presence") %>%
     mutate(creation_date=as.Date(Sys.Date()))%>% 
     rbind(.,blue_ind) %>% 
     arrange(Date)

   write.csv(bluewhale_master,glue("{outdir}/ramp_zone_indicators/bluewhale_ramp_zone34_indicator.csv"))
   }
  
  print("**************************************************************************************")
  } 


library(tidyverse)

date_range=seq(as.Date("2023-05-01"),Sys.Date(),by=1) %>% as.character()

# date_range=seq(as.Date("2020-05-31"),as.Date("2020-06-02"),by=1) %>% as.character()
# date_range=seq(Sys.Date()-30,Sys.Date(),by=1) %>% as.character()

ramp_zone(path=path,source_path = source_path,date_range=date_range)

