


a=template
a[values(a)!=i]=2

lat <- ncvar_get(nc.data,'lat'); lat <- lat[1,] ## get the lons in the netcdf
lon <- ncvar_get(nc.data,'lon'); lon <- lon[,1] ## get teh lats in the netcdf
data.var.point  <-  ncvar_get(nc.data,varname,start=c(1,1,1),
                              count=c(-1,-1,1),verbose=FALSE)

dat1=list()
dat1$x=lon
dat1$y=lat
dat1$z=t(data.var.point)

r <-raster(
  dat1$z,
  xmn=range(dat1$x)[1], xmx=range(dat1$x)[2],
  ymn=range(dat1$y)[1], ymx=range(dat1$y)[2], 
  crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
) %>% flip()

template=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products_final/rasters/2023/05/epac_2023-05-06.grd")
template[values(template)==0]=NA
template[values(template)>0]=1
test=cellStats(template,sum) ## 3875 pixels


random=seq(1:test)
# template=setValues(template,random)
template[values(template)==1]=random

sst=brick("/Users/heatherwelch/Downloads/sst_daily_roms_gfdl_1980_2100.nc")
sst_nc=nc_open("/Users/heatherwelch/Downloads/sst_daily_roms_gfdl_1980_2100.nc")

sstVar <- ncvar_get(sst_nc, 'sst') # Takes less than 10 mins
lon <- sst_nc$dim$longitude$vals; lat <- sst_nc$dim$latitude$vals; tsteps <- sst_nc$dim$time$vals
lon.manip <- ncvar_get(sst_nc, "lon"); lat.manip <- ncvar_get(sst_nc, "lat")

master=rasterToPoints(template) %>% as.data.frame() %>% 
  rename(id=pred_mean2) %>% 
  rename(lon=x) %>% 
  rename(lat=y)

master_extract=raster::extract(sst,master[,1:2]) %>% 
  as.data.frame()

master_extract[1:10,1:15]

dates=names(master_extract)%>% 
  gsub("X","",.) %>% 
  gsub(".12.00.00","",.) %>% 
  ymd(.)
  
  
for(i in 1:nrow(master_extract)){
  dat=master_extract[i,] %>% t() %>% as.data.frame()
  
  clim <- ts2clm(dat, date, climatologyPeriod=c("1980-01-01", "2009-12-31"), pctile=10)
  mcs <- detect_event(clim, coldSpells=T, date)
  mcs <- select(mcs[["event"]], event_no, date_start, date_peak, date_end, intensity_mean, intensity_max, intensity_cumulative, rate_onset, rate_decline)
  mcs$lat <- xyFromCell(clip,i)[2]; mcs$lon <- xyFromCell(clip,i)[1]
  mcs.list[[i]] <- mcs
  
}

nc="/Users/heatherwelch/Downloads/sst_daily_roms_gfdl_1980_2100.nc"
inpts=master
varname="sst"

getvarROMS <- function(nc,varname,inpts){
  
  inpts$dt <- as.POSIXct(inpts$dt, '%Y-%m-%d',tz='UTC')
  nc.data <- nc_open(nc, write=FALSE)
  lat <- ncvar_get(nc.data,'lat'); lat <- lat[1,]
  lon <- ncvar_get(nc.data,'lon'); lon <- lon[,1]
  nrows <- length(lat); ncols <- length(lon)
  yr <- ncvar_get(nc.data,'year'); mth <- ncvar_get(nc.data,'month'); day <- ncvar_get(nc.data,'day')
  tim <- as.POSIXct(paste(yr,mth,day,sep='-'),tz='UTC')
  desired.resolution = desired.resolution/2
  for (i in 1:nrow(inpts)){
    print(paste(varname,inpts$id[i],sep=' '))
      c <- which.min(abs(lon-inpts$lon[i]))
     
      r <- which.min(abs(lat-inpts$lat[i]))

        data.var.point  <-  ncvar_get(nc.data,varname,start=c(c,r,1),
                                      count=c(1,1,-1),verbose=FALSE)
        inpts[i,paste(varname)] <- data.var.point
  }
  nc_close(nc.data)
  return(inpts)
}

df=data.frame(t=as.Date(tim),temp=data.var.point)
test=ts2clm(data=df,climatologyPeriod=c("1980-01-01", "2009-12-31"), pctile=10)
mcs <- detect_event(test, coldSpells=T, as.Date(tim))

library(tidyverse)
library(vroom)
a=vroom("/Users/heatherwelch/Downloads/drive-download-20240304T234831Z-001/WEAR_3km_2020-05-08.csv")
aa=rasterFromXYZ(a[,c("lon","lat","ssh.mean")])
b=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-08/ssh_humpback.grd") %>% 
  rasterToPoints() %>% as.data.frame()
bb=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-08/ssh_humpback.grd") 
extent(bb)=extent(aa)
c=mask(bb,aa)
ggplot(a,aes(x=lon,y=lat,fill=ssh.mean))+
  geom_tile()

bb_mask=mask(bb,aa)
aa_mask=mask(aa,bb_mask)
c=bb_mask-aa

test=stack(bb_mask,aa_mask) %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  gather(layer,value,-c(x,y))
ggplot(test,aes(x=x,y=y,fill=value))+
  geom_tile()+
  facet_wrap(~layer)

sum_aa=cellStats(aa_mask,sum)
sum_bb=cellStats(bb_mask,sum)


aa_minus=aa_mask-0.154
bb_minus=bb_mask-0.035

test=stack(aa_minus,bb_minus) %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  gather(layer,value,-c(x,y))
ggplot(test,aes(x=x,y=y,fill=value))+
  geom_tile()+
  facet_wrap(~layer)

c=aa_minus-bb_minus

## new try #####
a=vroom("/Users/heatherwelch/Dropbox/OPC_Farallons/misc/drive-download-20240304T234831Z-001/WEAR_3km_2020-05-08.csv")
a_ssh=rasterFromXYZ(a[,c("lon","lat","ssh.mean")])
a_mld=rasterFromXYZ(a[,c("lon","lat","ild.mean")])
a_sst=rasterFromXYZ(a[,c("lon","lat","analysed_sst.mean")])
karin_stack=stack(a_ssh,a_mld,a_sst)
names(karin_stack)=c("karin_SSH","karin_MLD","karin_SST")

ssh=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-08/ssh_humpback.grd") 
mld=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-08/ild_humpback.grd") 
sst=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/daily_prediction_layers/2020-05-08/sst_humpback.grd") 
heather_stack=stack(ssh,mld,sst)  
names(heather_stack)=c("heather_SSH","heather_MLD","heather_SST")

extent(karin_stack)=extent(heather_stack)
karin_stack2=mask(karin_stack,heather_stack)
heather_stack2=mask(heather_stack,karin_stack)

full_stack=stack(karin_stack2,heather_stack2)
full_stack_pnts=rasterToPoints(full_stack) %>% as.data.frame()

ggplot(full_stack_pnts,aes(x=karin_SSH,y=heather_SSH))+geom_point()+
  coord_equal()+
  geom_abline(intercept = 0,slope = 1,color="red")

ggplot(full_stack_pnts,aes(x=karin_MLD,y=heather_MLD))+geom_point()+
  coord_equal()+
  geom_abline(intercept = 0,slope = 1,color="red")

ggplot(full_stack_pnts,aes(x=karin_SST,y=heather_SST))+geom_point()+
  coord_equal()+
  geom_abline(intercept = 0,slope = 1,color="red")
