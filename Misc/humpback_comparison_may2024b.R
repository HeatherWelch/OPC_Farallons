## ILD comparisons from four sources from 2020-05-02
#1: netcdf file: https://www.dropbox.com/scl/fi/rlmbs7jl2x1tjwkfan7g9/wcnrt_ild_05_daily_20110102_20240117.nc?rlkey=lkiqv765ngjgqx2kw0ih8rvkj&st=cigt6l1a&dl=0
#2: thredds server (how the operational tool gets data)
#3: csv from Karin
#4: raster output from operational tool (should be the exact same as #2 but double checking)

## for each 1-4, also comparing two methods of getting data onto the humpback grid
#1: raster::resample, nearest neighbor
#2: raster::extract

library(tidyverse)
library(raster)
library(ncdf4)
library(glue)
library(patchwork)

## define objects
dname="ild_05"
get_date="2020-05-02"

template_humpback_points=read.csv("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/static_variables/humpback/Grid_Nonrectangle_3km_WEAR_bathy.csv")[, c("lon180", "lat")] # points
template_humpback=read.csv("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/static_variables/humpback/Grid_Nonrectangle_3km_WEAR_bathy.csv")[, c("lon180", "lat")] %>%  # raster
  rasterFromXYZ()

### ---------------------------------------------------------------------------------------> get ild data ####
#1: netcdf file ####
nc.data=nc_open("/Users/heatherwelch/Downloads/wcnrt_ild_05_daily_20110102_20240117.nc")
print("grabbing dimensions and variables")
lat <- ncvar_get(nc.data,'lat_rho')
lon <- ncvar_get(nc.data,'lon_rho')
nrows <- length(lat); ncols <- length(lon)
time=ncvar_get(nc.data,"time")
tim <- as.POSIXct(time,tz='UTC',origin="2011-01-02")
tmp.array <- ncvar_get(nc.data, dname)
fillvalue <- ncatt_get(nc.data, dname, "_FillValue")
print("creating matrix for day of interest")
names=unlist(lapply(tim,function(x)gsub(" UTC","",x)))
index=grep(get_date,names)

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

#2: thredds server (how the operational tool gets data) ####
ref_date <- dmy('02-01-2011')
new_date <- as.Date(get_date)
days <- as.numeric(difftime(new_date, ref_date))

my_url = glue("https://oceanmodeling.ucsc.edu/thredds/dodsC/ccsra_2016a_phys_agg_derived_vars/fmrc/CCSRA_2016a_Phys_ROMS_Derived_Variables_Aggregation_best.ncd?{dname}[{days}:1:{days}][0:1:180][0:1:185],lat_rho[0:1:180][0:1:185],lon_rho[0:1:180][0:1:185],time[0:1:1]")
nc.data=nc_open(my_url)
print("grabbing dimensions and variables")
lat <- ncvar_get(nc.data,'lat_rho')
lon <- ncvar_get(nc.data,'lon_rho')
nrows <- length(lat); ncols <- length(lon)


tmp.array <- ncvar_get(nc.data, dname)
fillvalue <- ncatt_get(nc.data, dname, "_FillValue")
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
r_thredds=flip(r,2)

#3: csv from Karin ####
kf=vroom(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/misc/May2020_Updated_Density_Predictions/WEAR_3km_{get_date}.csv"))
r_karin=rasterFromXYZ(kf[,c("lon","lat","ild.mean")])

#4: raster output from operational tool (should be the exact same as #2 but double checking) ####
r_heather=raster(glue("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/{get_date}/ild_humpback.grd"))

### ---------------------------------------------------------------------------------------> comparisons ####
## 1. raw netcdf vs raw thredds
a=r_wcnrt-r_thredds ## zero difference

## 2. resampled netcdf vs extracted thredds (putting on humpback grid)
r_wcnrt_resample=raster::resample(r_wcnrt, template_humpback,method="ngb")  
r_thredds_extract=template_humpback_points %>% 
  mutate(ild=raster::extract(r_thredds,template_humpback_points)) %>% 
  rasterFromXYZ()

b=r_wcnrt_resample-r_thredds_extract ## zero difference

## 3. extracted netcdf vs resampled thredds (putting on humpback grid)
r_thredds_resample=raster::resample(r_thredds, template_humpback,method="ngb")  
r_wcnrt_extract=template_humpback_points %>% 
  mutate(ild=raster::extract(r_wcnrt,template_humpback_points)) %>% 
  rasterFromXYZ()

c=r_thredds_resample-r_wcnrt_extract ## zero difference

## 4. comparing thredds, wcnrt, heather, karin 
names(r_thredds_resample)="thredds"
names(r_wcnrt_extract)="netcdf"
names(r_heather)="heather"
names(r_karin)="karin"
full=stack(r_thredds_resample,r_wcnrt_extract,r_heather,r_karin) %>% 
  rasterToPoints() %>% as.data.frame()

plot1=ggplot(full,aes(x=thredds,y=netcdf))+
  geom_abline(intercept = 0,slope = 1,color="red")+
  geom_point()
plot2=ggplot(full,aes(x=thredds,y=heather))+
  geom_abline(intercept = 0,slope = 1,color="red")+
  geom_point()
plot3=ggplot(full,aes(x=thredds,y=karin))+
  geom_abline(intercept = 0,slope = 1,color="red")+
  geom_point()

plot4=ggplot(full,aes(x=netcdf,y=heather))+
  geom_abline(intercept = 0,slope = 1,color="red")+
  geom_point()
plot5=ggplot(full,aes(x=netcdf,y=karin))+
  geom_abline(intercept = 0,slope = 1,color="red")+
  geom_point()

plot6=ggplot(full,aes(x=heather,y=karin))+
  geom_abline(intercept = 0,slope = 1,color="red")+
  geom_point()

(plot1+plot2+plot3)/(plot4+plot5+plot6)
