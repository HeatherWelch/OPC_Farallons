box=c(-134,-115.5,30,48) ## xmin,xmax,ymin,ymax ** always express xmin xmax here in 0-360 **


subsetLatLon <- function(conn,var,box,date_position,depth_start,ndepths){
  #returns a raster of data extracted from a netcdf file or opendap connection
  #parameters
  ## conn: netcdf file or remote opendap connection, obtained with nc_open
  ## box: region to extract
  ##	vector of xmin,xmax,ymin,ymax ** xmin xmax always expressed in 0-360 degrees **
  ##	data can be 0-360 or -180- 180, latitude can be north to south and can extract across dateline
  ## example:  box=c(100,295,-60,60)
  ## date_position: date index to extract
  ## depth_start: depth index to start the extract, or if no depth set to -1
  ## ndepths: number of depths to extract, ignored if depth_start set to -1
  ## subsetting lat
    lat_start=which.min(abs(conn$dim$lat$vals-box[4]))
    lat_end=which.min(abs(conn$dim$lat$vals-box[3]))
    lat_n_to_s=TRUE
 
  lat <- conn$dim$lat$vals[lat_start:lat_end]
  nrows <- length(lat);

      x1=box[1] 
      x2=box[2]
      lon_start1=which.min(abs(conn$dim$lon$vals-x1))
      lon_end1=which.min(abs(conn$dim$lon$vals-x2))
      lon1 <- conn$dim$lon$vals[lon_start1:lon_end1]
      ncols1 <- length(lon1)
      tmp.array1 <- ncvar_get(conn, varid=var,c(lon_start1,lat_start,date_position),c(ncols1,nrows,1))
      r <- raster(t(tmp.array1),xmn=range(lon1)[1],xmx=range(lon1)[2],ymn=range(lat)[1],ymx=range(lat)[2],crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    

  if(!lat_n_to_s){
    return(flip(r))
  }
  else{
    return(r)
  }
}

a=raster::resample(r,template)
b=rasterToPoints(r) %>% as.data.frame() %>% 
  mutate(layer=log(layer+.001))

ggplot()+
  geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)),
               aes(x=long, y = lat, group=group),color="black",fill="grey")+
  geom_tile(data=b,aes(x = x, y = y, fill=layer))+
scale_fill_gradientn(colours = pals::parula(100),na.value="black")+
 
  coord_cartesian(xlim=c(-136,-110),ylim=c(25,50))
