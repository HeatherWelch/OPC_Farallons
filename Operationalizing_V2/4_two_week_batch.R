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

two_week <- function(path,source_path,date_range){
  
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

  ############ 2. Define time and dynamic directories
  for(date in date_range){
    get_date=date
    start_date=as.Date(get_date)-13
    print(get_date)
    
    year=year(get_date)
    month=month(get_date) %>% str_pad(.,2,side="left",pad=0)
    
    sub_range=seq(start_date,as.Date(get_date),by=1)
    
    print("**************************************************************************************")
    print(paste("Starting two week prediction for ",get_date,". Time is ",Sys.time(),sep=""))
    
    ## epac ####
    name="epac"
    if(file.exists(glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv"))){
      metadata=read.csv(glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv")) %>% 
        pull(n_days_w_data) %>% mean()
    }else{metadata=0}
    
    if(metadata!=14){
      if(metadata==0){
        print(glue("Two week raster doesn't exist for {name}, creating"))}
      else if(metadata>0&metadata<14){
        print(glue("Two week raster was missing data for {name}, re-creating"))}
    
    ras=list.files(glue("{outdir}/rasters"),pattern=name,full.names = T,recursive = T) %>% 
      grep(".grd",.,value=T) %>% 
      grep(paste(sub_range,collapse = "|"),.,value=T)
    
    if(length(ras)>=1){
    ras_dates=ras %>% 
      gsub(glue("{outdir}/rasters"),"",.) %>% 
      substr(.,nchar(.)-13,nchar(.)-4)
    
    ## raster
    ras_mean=stack(ras) %>% mean()
    writeRaster(ras_mean,glue("{outdir}/two_week_rasters/{year}/{month}/{name}_start{start_date}-end{get_date}.grd"),overwrite=T)
    
    ## map
    df_plot=rasterToPoints(ras_mean) %>% 
      as.data.frame()
    
    pred_map=ggplot()+
      geom_tile(data=df_plot,aes(x = x, y = y, fill=layer))+
      scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
      geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
      theme_classic()+xlab(NULL)+ylab(NULL)+
      coord_sf(xlim = c(-127, -115.5), ylim = c(30,46),expand=F)+
      ggtitle(glue("{name} \n{start_date} to {get_date}"))+
      theme(legend.position = "bottom",
            legend.key.width = unit(1, 'cm'))
    
    png(glue("{outdir}/two_week_maps/{year}/{month}/{name}_start{start_date}-end{get_date}.png"),width=8,height=11,units='cm',res=400,type = "cairo")
    par(ps=10)
    par(mar=c(0,0,0,0))
    par(cex=1)
    print({pred_map})
    # gg_hm
    dev.off()
    
    ## metadata
    metadata=data.frame(
      prediction_date=get_date,
      attempted_start=start_date,
      attempted_end=get_date,
      actual_start=ras_dates[1],
      actual_end=ras_dates[length(ras_dates)],
      n_days_w_data=length(ras_dates)
    )
    
    write.csv(metadata,glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv"))
    }
    }
    
    ## bluewhale ####
    name="bluewhale"
    if(file.exists(glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv"))){
      metadata=read.csv(glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv")) %>% 
        pull(n_days_w_data) %>% mean()
    }else{metadata=0}
    
    if(metadata!=14){
      if(metadata==0){
        print(glue("Two week raster doesn't exist for {name}, creating"))}
      else if(metadata>0&metadata<14){
        print(glue("Two week raster was missing data for {name}, re-creating"))}
      
    ras=list.files(glue("{outdir}/rasters"),pattern=name,full.names = T,recursive = T) %>% 
      grep(".grd",.,value=T) %>% 
      grep(paste(sub_range,collapse = "|"),.,value=T)
    
    if(length(ras)>=1){
      ras_dates=ras %>% 
        gsub(glue("{outdir}/rasters"),"",.) %>% 
        substr(.,nchar(.)-13,nchar(.)-4)
      
      ## raster
      ras_mean=stack(ras) %>% mean()
      writeRaster(ras_mean,glue("{outdir}/two_week_rasters/{year}/{month}/{name}_start{start_date}-end{get_date}.grd"),overwrite=T)
      
      ## map
      df_plot=rasterToPoints(ras_mean) %>% 
        as.data.frame()
      
      pred_map=ggplot()+
        geom_tile(data=df_plot,aes(x = x, y = y, fill=layer))+
        scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
        geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
        theme_classic()+xlab(NULL)+ylab(NULL)+
        coord_sf(xlim = c(-134, -115.5), ylim = c(30,46),expand=F)+
        ggtitle(glue("{name} \n{start_date} to {get_date}"))+
        theme(legend.position = "bottom",
              legend.key.width = unit(1, 'cm'))
      
      png(glue("{outdir}/two_week_maps/{year}/{month}/{name}_start{start_date}-end{get_date}.png"),width=8,height=11,units='cm',res=400,type = "cairo")
      par(ps=10)
      par(mar=c(0,0,0,0))
      par(cex=1)
      print({pred_map})
      # gg_hm
      dev.off()
      
      ## metadata
      metadata=data.frame(
        prediction_date=get_date,
        attempted_start=start_date,
        attempted_end=get_date,
        actual_start=ras_dates[1],
        actual_end=ras_dates[length(ras_dates)],
        n_days_w_data=length(ras_dates)
      )
      
      write.csv(metadata,glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv"))
    }
    }
    
    ## anchovy ####
    name="anchovy"
    if(file.exists(glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv"))){
      metadata=read.csv(glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv")) %>% 
        pull(n_days_w_data) %>% mean()
    }else{metadata=0}
    
    if(metadata!=14){
      if(metadata==0){
        print(glue("Two week raster doesn't exist for {name}, creating"))}
      else if(metadata>0&metadata<14){
        print(glue("Two week raster was missing data for {name}, re-creating"))}
      
    ras=list.files(glue("{outdir}/rasters"),pattern=name,full.names = T,recursive = T) %>% 
      grep(".grd",.,value=T) %>% 
      grep(paste(sub_range,collapse = "|"),.,value=T)
    
    if(length(ras)>=1){
      ras_dates=ras %>% 
        gsub(glue("{outdir}/rasters"),"",.) %>% 
        substr(.,nchar(.)-13,nchar(.)-4)
      
      ## raster
      ras_mean=stack(ras) %>% mean()
      writeRaster(ras_mean,glue("{outdir}/two_week_rasters/{year}/{month}/{name}_start{start_date}-end{get_date}.grd"),overwrite=T)
      
      ## map
      df_plot=rasterToPoints(ras_mean) %>% 
        as.data.frame()
      
      pred_map=ggplot()+
        geom_tile(data=df_plot,aes(x = x, y = y, fill=layer))+
        scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
        geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
        theme_classic()+xlab(NULL)+ylab(NULL)+
        coord_sf(xlim = c(-134, -115.5), ylim = c(30,46),expand=F)+
        ggtitle(glue("{name} \n{start_date} to {get_date}"))+
        theme(legend.position = "bottom",
              legend.key.width = unit(1, 'cm'))
      
      png(glue("{outdir}/two_week_maps/{year}/{month}/{name}_start{start_date}-end{get_date}.png"),width=8,height=11,units='cm',res=400,type = "cairo")
      par(ps=10)
      par(mar=c(0,0,0,0))
      par(cex=1)
      print({pred_map})
      # gg_hm
      dev.off()
      
      ## metadata
      metadata=data.frame(
        prediction_date=get_date,
        attempted_start=start_date,
        attempted_end=get_date,
        actual_start=ras_dates[1],
        actual_end=ras_dates[length(ras_dates)],
        n_days_w_data=length(ras_dates)
      )
      
      write.csv(metadata,glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv"))
    }
    }
    
    ## humpback ####
    name="humpback"
    if(file.exists(glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv"))){
      metadata=read.csv(glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv")) %>% 
        pull(n_days_w_data) %>% mean()
    }else{metadata=0}
    
    if(metadata!=14){
      if(metadata==0){
        print(glue("Two week raster doesn't exist for {name}, creating"))}
      else if(metadata>0&metadata<14){
        print(glue("Two week raster was missing data for {name}, re-creating"))}
      
    ras=list.files(glue("{outdir}/rasters"),pattern=name,full.names = T,recursive = T) %>% 
      grep(".grd",.,value=T) %>% 
      grep(paste(sub_range,collapse = "|"),.,value=T)
    
    if(length(ras)>=1){
      ras_dates=ras %>% 
        gsub(glue("{outdir}/rasters"),"",.) %>% 
        substr(.,nchar(.)-13,nchar(.)-4)
      
      ## raster
      ras_mean=stack(ras) %>% mean()
      writeRaster(ras_mean,glue("{outdir}/two_week_rasters/{year}/{month}/{name}_start{start_date}-end{get_date}.grd"),overwrite=T)
      
      ## map
      df_plot=rasterToPoints(ras_mean) %>% 
        as.data.frame()
      
      pred_map=ggplot()+
        geom_tile(data=df_plot,aes(x = x, y = y, fill=layer))+
        scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
        geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
        theme_classic()+xlab(NULL)+ylab(NULL)+
        coord_sf(xlim = c(-127, -115.5), ylim = c(30,46),expand=F)+
        ggtitle(glue("{name} \n{start_date} to {get_date}"))+
        theme(legend.position = "bottom",
              legend.key.width = unit(1, 'cm'))
      
      png(glue("{outdir}/two_week_maps/{year}/{month}/{name}_start{start_date}-end{get_date}.png"),width=8,height=11,units='cm',res=400,type = "cairo")
      par(ps=10)
      par(mar=c(0,0,0,0))
      par(cex=1)
      print({pred_map})
      # gg_hm
      dev.off()
      
      ## metadata
      metadata=data.frame(
        prediction_date=get_date,
        attempted_start=start_date,
        attempted_end=get_date,
        actual_start=ras_dates[1],
        actual_end=ras_dates[length(ras_dates)],
        n_days_w_data=length(ras_dates)
      )
      
      write.csv(metadata,glue("{outdir}/two_week_metadata/{year}/{month}/{name}_metadata_{get_date}.csv"))
    }
    }
  
  print("**************************************************************************************")
  } 
}

library(tidyverse)

date_range=seq(as.Date("2023-03-01"),Sys.Date(),by=1) %>% as.character()

# date_range=seq(as.Date("2020-05-31"),as.Date("2020-06-02"),by=1) %>% as.character()
# date_range=seq(Sys.Date()-30,Sys.Date(),by=1) %>% as.character()

two_week(path=path,source_path = source_path,date_range=date_range)

