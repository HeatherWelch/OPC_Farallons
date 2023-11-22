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

most_recent <- function(path,source_path){
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)

  envdir=glue("{path}/daily_prediction_layers")
  outdir <- glue("{path}/end_products_final")
  staticdir=glue("{path}/static_variables")
  # temp=glue("{path}/raw_roms_data")
  moddir=glue("{path}/models")
  intermediatedir=glue("{path}/intermediate")
  
  # flyersdir=glue("{outdir}/flyers")
   rastersdir=glue("{outdir}/rasters")
   mapssdir=glue("{outdir}/maps")
   latestdir=glue("{outdir}/latest")
  # latestweekdir=glue("{outdir}/latest_week")
  # latestweeksmootheddir=glue("{outdir}/latest_week_smoothed")
   
   ## remove everything in latest folder ####
   to_remove=list.files(latestdir,recursive = T,full.names = T)
   file.remove(to_remove)
  
   ## latest epac metadata ####
   target_dir=glue("{outdir}/epac_metadata")
   final_dir=glue("{latestdir}/epac_metadata")
   files=list.files(target_dir,full.names = T) 
   files_dates=files %>% 
     gsub(glue("{outdir}/epac_metadata/metadata_"),"",.) %>% 
     gsub(".csv","",.) %>% sort() 
   move_files=files_dates[(length(files_dates)-30):length(files_dates)] %>% 
     lapply(.,function(x)glue("{outdir}/epac_metadata/metadata_{x}.csv")) %>% 
     unlist()
   
   lapply(move_files,function(x)file.copy(x,final_dir))
   
   ## latest two week metadata ####
   target_dir=glue("{outdir}/two_week_metadata")
   final_dir=glue("{latestdir}/two_week_metadata")
   files=list.files(target_dir,full.names = T) 
   
   names=c("anchovy","humpback","bluewhale","epac")
   for(i in 1:length(names)){
     files_dates=files %>% 
       grep(names[i],.,value=T) %>% 
       gsub(glue("{target_dir}/{names[i]}_metadata_"),"",.) %>% 
       gsub(".csv","",.) %>% sort() 
     move_files=files_dates[(length(files_dates)-30):length(files_dates)] %>% 
       lapply(.,function(x)glue("{target_dir}/{names[i]}_metadata_{x}.csv")) %>% 
       unlist()
     
     lapply(move_files,function(x)file.copy(x,final_dir))
   }
   
   ## latest maps ####
   target_dir=glue("{outdir}/maps")
   final_dir=glue("{latestdir}/maps")
   files=list.files(target_dir,full.names = T) 
   
   names=c("anchovy","humpback","bluewhale","epac")
   for(i in 1:length(names)){
   files_dates=files %>% 
     grep(names[i],.,value=T) %>% 
     gsub(glue("{target_dir}/{names[i]}_"),"",.) %>% 
     gsub(".png","",.) %>% sort() 
   move_files=files_dates[(length(files_dates)-30):length(files_dates)] %>% 
     lapply(.,function(x)glue("{target_dir}/{names[i]}_{x}.png")) %>% 
     unlist()
   
   lapply(move_files,function(x)file.copy(x,final_dir))
   }
   
   ## latest two week maps ####
   target_dir=glue("{outdir}/two_week_maps")
   final_dir=glue("{latestdir}/two_week_maps")
   files=list.files(target_dir,full.names = T) 
   
   names=c("anchovy","humpback","bluewhale","epac")
   for(i in 1:length(names)){
     files_dates=files %>% 
       grep(names[i],.,value=T) %>% 
       gsub(glue("{target_dir}/{names[i]}_"),"",.) %>% 
       gsub(".png","",.) %>% sort() 
     move_files=files_dates[(length(files_dates)-30):length(files_dates)] %>% 
       lapply(.,function(x)glue("{target_dir}/{names[i]}_{x}.png")) %>% 
       unlist()
     
     lapply(move_files,function(x)file.copy(x,final_dir))
   }
   
   ## latest rasters ####
   target_dir=glue("{outdir}/rasters")
   final_dir=glue("{latestdir}/rasters")
   files=list.files(target_dir,full.names = T,pattern=".grd") 
   
   names=c("anchovy","humpback","bluewhale","epac")
   for(i in 1:length(names)){
     files_dates=files %>% 
       grep(names[i],.,value=T) %>% 
       gsub(glue("{target_dir}/{names[i]}_"),"",.) %>% 
       gsub(".grd","",.) %>% sort() 
     move_files=files_dates[(length(files_dates)-30):length(files_dates)] %>% 
       lapply(.,function(x)glue("{target_dir}/{names[i]}_{x}.grd")) %>% ##grd
       unlist()
     
     lapply(move_files,function(x)file.copy(x,final_dir))
     
     move_files=files_dates[(length(files_dates)-30):length(files_dates)] %>% 
       lapply(.,function(x)glue("{target_dir}/{names[i]}_{x}.gri")) %>% ##gri
       unlist()
     
     lapply(move_files,function(x)file.copy(x,final_dir))
   }

   ## latest two week rasters ####
   target_dir=glue("{outdir}/two_week_rasters")
   final_dir=glue("{latestdir}/two_week_rasters")
   files=list.files(target_dir,full.names = T,pattern=".grd") 
   
   names=c("anchovy","humpback","bluewhale","epac")
   for(i in 1:length(names)){
     files_dates=files %>% 
       grep(names[i],.,value=T) %>% 
       gsub(glue("{target_dir}/{names[i]}_"),"",.) %>% 
       gsub(".grd","",.) %>% sort() 
     move_files=files_dates[(length(files_dates)-30):length(files_dates)] %>% 
       lapply(.,function(x)glue("{target_dir}/{names[i]}_{x}.grd")) %>% ##grd
       unlist()
     
     lapply(move_files,function(x)file.copy(x,final_dir))
     
     move_files=files_dates[(length(files_dates)-30):length(files_dates)] %>% 
       lapply(.,function(x)glue("{target_dir}/{names[i]}_{x}.gri")) %>% ##gri
       unlist()
     
     lapply(move_files,function(x)file.copy(x,final_dir))
   }
   
  } 


library(tidyverse)
most_recent(path=path,source_path = source_path)

