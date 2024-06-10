## comparing karin and my models
library(tidyverse)
library(vroom)
library(raster)
library(patchwork)
library(glue)

outdir="/Users/heatherwelch/Dropbox/OPC_Farallons/misc/humpback_May6_2024b";dir.create(outdir)

## predictions ####

me=list.files("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products_final/rasters/2020/05",
              pattern=".grd",full.names = T)

kf=vroom("/Users/heatherwelch/Dropbox/OPC_Farallons/misc/May2020_Updated_Density_Predictions/Mn_3km_2020-05-02_to_2020-05-30_bidaily_dens.csv") %>% 
  dplyr::select(c(pixel,mlat,mlon,
                  `76.dens.2020.05.02`,
                  `76.dens.2020.05.04`,
                  `76.dens.2020.05.06`,
                  `76.dens.2020.05.08`,
                  `76.dens.2020.05.10`,
                  `76.dens.2020.05.12`,
                  `76.dens.2020.05.14`,
                  `76.dens.2020.05.16`,
                  `76.dens.2020.05.18`,
                  `76.dens.2020.05.20`,
                  `76.dens.2020.05.22`,
                  `76.dens.2020.05.24`,
                  `76.dens.2020.05.26`,
                  `76.dens.2020.05.28`,
                  `76.dens.2020.05.30`)) %>% 
  rename('humpback_2020-05-02'=`76.dens.2020.05.02`,
         'humpback_2020-05-04'=`76.dens.2020.05.04`,
         'humpback_2020-05-06'=`76.dens.2020.05.06`,
         'humpback_2020-05-08'=`76.dens.2020.05.08`,
         'humpback_2020-05-10'=`76.dens.2020.05.10`,
         'humpback_2020-05-12'=`76.dens.2020.05.12`,
         'humpback_2020-05-14'=`76.dens.2020.05.14`,
         'humpback_2020-05-16'=`76.dens.2020.05.16`,
         'humpback_2020-05-18'=`76.dens.2020.05.18`,
         'humpback_2020-05-20'=`76.dens.2020.05.20`,
         'humpback_2020-05-22'=`76.dens.2020.05.22`,
         'humpback_2020-05-24'=`76.dens.2020.05.24`,
         'humpback_2020-05-26'=`76.dens.2020.05.26`,
         'humpback_2020-05-28'=`76.dens.2020.05.28`,
         'humpback_2020-05-30'=`76.dens.2020.05.30`)

cols=names(kf) %>% 
  grep("pixel",.,invert = T,value=T)%>% 
  grep("mlat",.,invert = T,value=T)%>% 
  grep("mlon",.,invert = T,value=T)

for(i in 1:length(cols)){
  print(cols[i])
  kf_ras=rasterFromXYZ(kf[,c("mlon","mlat",cols[i])])
  
  me_ras=me %>% 
    grep(cols[i],.,value = T) %>% 
    raster()
  
  extent(kf_ras)=extent(me_ras)
  kf_ras2=mask(kf_ras,me_ras)
  me_ras2=mask(me_ras,kf_ras)
  names(kf_ras2)="Karin"
  names(me_ras2)="Heather"
  
  diff=(kf_ras2-me_ras2) %>% 
    rasterToPoints() %>% as.data.frame()
  together=stack(kf_ras2,me_ras2) %>% 
    rasterToPoints() %>% as.data.frame()
  
  a=ggplot(together,aes(x=x,y=y,fill=Karin))+
    geom_tile()+
    theme_classic()+
    scale_fill_gradientn(colours = pals::parula(100))+
    ggtitle(cols[i])
  
  b=ggplot(together,aes(x=x,y=y,fill=Heather))+
    geom_tile()+
    theme_classic()+
    scale_fill_gradientn(colours = pals::parula(100))+
    ggtitle(cols[i])
  
  c=ggplot(diff,aes(x=x,y=y,fill=layer))+
    geom_tile()+
    theme_classic()+
    scale_fill_gradient2("difference")+
    ggtitle(cols[i])
  
  png(glue("{outdir}/humpback_{cols[i]}.png"),width=20,height=8,units='cm',res=400,type = "cairo")
  print({a+b+c})
  # gg_hm
  dev.off()
  
}

         
## environmental data ####
dates=seq(as.Date("2020-05-02"),as.Date("2020-05-30"),by=2)

for(i in 1:length(dates)){
  print(dates[i])
  
  kf=vroom(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/misc/May2020_Updated_Density_Predictions/WEAR_3km_{dates[i]}.csv"))
  a_ssh=rasterFromXYZ(kf[,c("lon","lat","ssh.mean")])
  a_mld=rasterFromXYZ(kf[,c("lon","lat","ild.mean")])
  a_sst=rasterFromXYZ(kf[,c("lon","lat","analysed_sst.mean")])
  karin_stack=stack(a_ssh,a_mld,a_sst)
  names(karin_stack)=c("karin_SSH","karin_MLD","karin_SST")
  
  ssh=raster(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/{dates[i]}/ssh_humpback.grd"))
  mld=raster(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/{dates[i]}/ild_humpback.grd"))
  sst=raster(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/{dates[i]}/sst_humpback.grd"))
  heather_stack=stack(ssh,mld,sst)  
  names(heather_stack)=c("heather_SSH","heather_MLD","heather_SST")
  
  extent(karin_stack)=extent(heather_stack)
  karin_stack2=mask(karin_stack,heather_stack)
  heather_stack2=mask(heather_stack,karin_stack)
  
  full_stack=stack(karin_stack2,heather_stack2)
  full_stack_pnts=rasterToPoints(full_stack) %>% as.data.frame()
  
  a=ggplot(full_stack_pnts,aes(x=karin_SSH,y=heather_SSH))+geom_point()+
    coord_equal()+
    geom_abline(intercept = 0,slope = 1,color="red")+
    ggtitle(glue("SSH {dates[i]}"))
  
  b=ggplot(full_stack_pnts,aes(x=karin_MLD,y=heather_MLD))+geom_point()+
    coord_equal()+
    geom_abline(intercept = 0,slope = 1,color="red")+
    ggtitle(glue("MLD {dates[i]}"))
  
  c=ggplot(full_stack_pnts,aes(x=karin_SST,y=heather_SST))+geom_point()+
    coord_equal()+
    geom_abline(intercept = 0,slope = 1,color="red")+
    ggtitle(glue("SST {dates[i]}"))
  
  png(glue("{outdir}/environment_{dates[i]}_ngb.png"),width=20,height=8,units='cm',res=400,type = "cairo")
  print({a+b+c})
  # gg_hm
  dev.off()
  
}

## ild 2020-05-02 ####
kf=vroom(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/misc/May2020_Updated_Density_Predictions/WEAR_3km_2020-05-02.csv"))
a_ssh=rasterFromXYZ(kf[,c("lon","lat","ssh.mean")])
a_mld=rasterFromXYZ(kf[,c("lon","lat","ild.mean")])
a_sst=rasterFromXYZ(kf[,c("lon","lat","analysed_sst.mean")])
karin_stack=stack(a_ssh,a_mld,a_sst)
names(karin_stack)=c("karin_SSH","karin_MLD","karin_SST")

ssh=raster(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-02/ssh_humpback.grd"))
mld=raster(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-02/ild_humpback.grd"))
sst=raster(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-02/sst_humpback.grd"))
heather_stack=stack(ssh,mld,sst)  
names(heather_stack)=c("heather_SSH","heather_MLD","heather_SST")

wcnrt=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/misc/May2020_WCNRT/ild_2020-05-02.grd")
names(wcnrt)="wcnrt"

extent(karin_stack)=extent(heather_stack)
extent(wcnrt)=extent(heather_stack)
karin_stack2=mask(karin_stack,heather_stack)
heather_stack2=mask(heather_stack,karin_stack)
wcnrt2=mask(wcnrt,karin_stack[[1]])

full_stack=stack(karin_stack2,heather_stack2,wcnrt2)
full_stack_pnts=rasterToPoints(full_stack) %>% as.data.frame()

a=ggplot(full_stack_pnts,aes(x=karin_MLD,y=wcnrt))+geom_point()+
  coord_equal()+
  geom_abline(intercept = 0,slope = 1,color="red")+
  ggtitle(glue("SSH {dates[i]}"))

b=ggplot(full_stack_pnts,aes(x=karin_MLD,y=heather_MLD))+geom_point()+
  coord_equal()+
  geom_abline(intercept = 0,slope = 1,color="red")+
  ggtitle(glue("MLD {dates[i]}"))

c=ggplot(full_stack_pnts,aes(x=heather_MLD,y=wcnrt))+geom_point()+
  coord_equal()+
  geom_abline(intercept = 0,slope = 1,color="red")+
  ggtitle(glue("SST {dates[i]}"))

## wcnrt ####
dname="ild_05"
day_nc="2020-05-02"
get_date=day_nc
var=dname
template_humpback_points=read.csv(glue("{staticdir}/humpback/Grid_Nonrectangle_3km_WEAR_bathy.csv"))[, c("lon180", "lat")]
template_humpback=read.csv(glue("{staticdir}/humpback/Grid_Nonrectangle_3km_WEAR_bathy.csv"))[, c("lon180", "lat")] %>% 
  rasterFromXYZ()

## from netcdf
  nc.template=nc_open("/Users/heatherwelch/Downloads/wcnrt_ild_05_daily_20110102_20240117.nc")
  nc.data=nc_open("/Users/heatherwelch/Downloads/wcnrt_ild_05_daily_20110102_20240117.nc")
  print("grabbing dimensions and variables")
  lat <- ncvar_get(nc.template,'lat_rho')
  lon <- ncvar_get(nc.template,'lon_rho')
  nrows <- length(lat); ncols <- length(lon)
  # yr <- ncvar_get(nc.data,'year'); mth <- ncvar_get(nc.data,'month'); day <- ncvar_get(nc.data,'day')
  time=ncvar_get(nc.template,"time")
  # tim <- as.POSIXct(paste(yr,mth,day,sep='-'),tz='UTC')
  tim <- as.POSIXct(time,tz='UTC',origin="2011-01-02")
  tmp.array <- ncvar_get(nc.data, dname)
  fillvalue <- ncatt_get(nc.data, dname, "_FillValue")
  print("creating matrix for day of interest")
  names=unlist(lapply(tim,function(x)gsub(" UTC","",x)))
  index=grep(day_nc,names)

    tmp.array.day=tmp.array[,,index]
    tmp.array.day[tmp.array.day==fillvalue$value]=NA #setting fill value
    
    dat1=list()
    dat1$x=lon
    dat1$y=lat
    dat1$z=t(tmp.array.day)
    
    r <-raster(
      dat1$z,
      xmn=range(dat1$x)[1], xmx=range(dat1$x)[2],
      ymn=range(dat1$y)[1], ymx=range(dat1$y)[2], 
      crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )
    r_wcnrt=flip(r,2)
    
    ##from thredds
    
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
    r2_thredds=flip(r,2)
    
    ## comparisons
    
    r3_thredds <- raster::resample(r2_thredds, template_humpback,method="ngb")  
    extent(r3_thredds)=extent(template_humpback)
    
    r3_wcnrt <- raster::resample(r_wcnrt, template_humpback,method="ngb")  
    extent(r3_wcnrt)=extent(template_humpback)
    
    dif=r2_thredds-r_wcnrt
    diff_hump=r3_thredds-r3_wcnrt
    
    opc=raster(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-02/ild_humpback.grd"))
   diff_opc=r3_thredds-opc
   
   ## points
   r3_thredds_points=template_humpback_points %>% 
     mutate(ild=raster::extract(r2_thredds,template_humpback_points)) %>% 
     rasterFromXYZ()
   r3_thredds_clip=mask(r3_thredds,r3_thredds_points)
   diff_points=r3_thredds_points-r3_thredds_clip
   