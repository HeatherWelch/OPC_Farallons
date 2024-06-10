## deleting all humpback data


path="/Users/EcoCast/Dropbox/OPC_Farallons/operationalization"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
# source_path="/Users/heatherwelch/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V1"
source_path="/Users/EcoCast/Dropbox/OPC_Farallons/github/OPC_Farallons/Operationalizing_V2"
source(paste0(source_path,"/loadlib-new.R"),chdir=T)

## enviro data: ####

envdir=glue("{path}/daily_prediction_layers")

a=list.files(envdir,pattern="ild_humpback",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(envdir,pattern="ssh_humpback",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(envdir,pattern="sst_humpback",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(envdir,pattern="ssh.grd",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(envdir,pattern="ssh.gri",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(envdir,pattern="ild.grd",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(envdir,pattern="ild.gri",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

## humpback data data: ####
outdir <- glue("{path}/end_products_final")
rastersdir=glue("{outdir}/rasters")
mapssdir=glue("{outdir}/maps")
two_week_rastersdir=glue("{outdir}/two_week_rasters")
two_week_metadir=glue("{outdir}/two_week_metadata")

a=list.files(rastersdir,pattern="humpback",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(mapssdir,pattern="humpback",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(two_week_rastersdir,pattern="humpback",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))

a=list.files(two_week_metadir,pattern="humpback",recursive = T,full.names = T)
lapply(a,function(x)file.remove(x))
