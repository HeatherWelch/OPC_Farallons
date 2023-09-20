library(gbm)
library(dismo)
library(viridis)
library(caret)
library(mlbench)
require(matrixStats)
require(fields)
require(reshape2)
require(class)
require(matlab)
require(maps)
#library(spatstat)


setwd('~/Desktop/NOAA_ERD/Projects/ROMs_Data/Elith_Supplement/')
source("brt.functions.R")


#####
colscatter <- function(data, no.colors, minval, maxval){
    o <- seq(maxval, minval, length = no.colors+1)
    o1 <- (o + (o[2]-o[1])/2)[1:(length(o)-1)]
    #c <- rev(tim.colors(no.colors))
    c <- rev(viridis(no.colors))
    na_ind <- is.na(data)
    test <- as.matrix(data)
    test_no_na <- test[na_ind==FALSE,]
    train <- as.matrix(o1)
    cl <- as.matrix(1:no.colors)
    colind <- knn(as.matrix(train), as.matrix(test_no_na), cl)
    v = NULL
    v$ind <- c[colind]
    test[na_ind==FALSE] <- v$ind
    v$ind <- test
    v$ind <- replace(v$ind, v$ind=="NaN" , NA)
    v$bar <- c
    return(v)
}

###################################################################################################

############## MODEL OUTPUT
dir<-"~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/BRT_results/EPAC/may_vars_all"
model_output_wd<-"~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/BRT_results/EPAC/may_vars_all/Model_predictions"

#dir<-"~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/BRT_results/EPAC/may_vars_trimmed"
#model_output_wd<-"~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/BRT_results/EPAC/may_vars_trimmed/Model_predictions"

#dir<-"~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/BRT_results/EPAC/published"
#model_output_wd<-"~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/BRT_results/EPAC/published/Model_predictions"


setwd(dir)
load( file="combined_models.Rdata")
model_output<-combined_models

######################################## LOAD DATA 
setwd("~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/datafiles")
load(file="all_krill_vars_matched.Rdata")
model.data2<-all_krill_vars_matched












########### CHANGE THESE THIGNS BASED ON KRILL 
max_z_value<-11.5
all_krill_vars_matched$E..pacifica<- replace(all_krill_vars_matched$E..pacifica, all_krill_vars_matched$E..pacifica>max_z_value, max_z_value)
C<-colscatter(all_krill_vars_matched$E..pacifica,64 ,min=0, max= max_z_value)

Variable<-"CPUE EPAC" 
numb_models<-50 ###### 


#############################################plot all years  
all_predictions_mn<-stack()
all_predictions_sd<-stack()
yr_mo<-NULL
years<-2002:2020 
months<-c("04","05","06","07","08")

for(i in 1:length(years)){
print(i)
  
for(xx in 1:length(months)){
  mon<- months[xx]
#### bathy and may variables #######
setwd(paste("~/Desktop/NOAA_ERD/Projects/ROMs_Data/ROMS_predict_monthly/" ,years[i],"-" ,mon, sep=""))
z_sd<-raster("z_sd.grd")
Distance_from_shore<-raster("Distance_from_shore.grd")
distance_to_canyon<-raster("distance_to_canyon.grd")
z<-raster("z.grd")
z<-replace(z, z> 0, NA)
z<-log10(abs(z))
su<-raster("su.grd")
svstr<-raster("svstr.grd")
sv<-raster("sv.grd")
sustr<-raster("sustr.grd")
bv<-raster("bv.grd")
curl<-raster("curl.grd")
ssh<-raster("ssh.grd")
ssh_sd_0.3<-raster("ssh_sd_0.3.grd")
sst<-raster("sst.grd")
sst_sd_0.3<-raster("sst_sd_0.3.grd")
ild<-raster("ild.grd")
#lat<-raster("lat.grd")
#EKE<-raster("EKE.grd")

xy_predict<- as.data.frame(z, xy=TRUE)[,-3]

layers<-cbind( as.data.frame(z),as.data.frame(su),as.data.frame(svstr),as.data.frame(bv),as.data.frame(curl),as.data.frame(z_sd),as.data.frame(Distance_from_shore),as.data.frame(distance_to_canyon),as.data.frame(ssh), as.data.frame(sst_sd_0.3),  as.data.frame(sv),  as.data.frame(ild), as.data.frame(sst), as.data.frame(ssh_sd_0.3),as.data.frame(sustr))#as.data.frame(EKE),as.data.frame(lat),
colnames(layers)<-c("z", "su", "svstr", "bv", "curl",  "z_sd","Distance_from_shore","distance_to_canyon","ssh", "sst_sd_0.3","sv" ,"ild"  ,"sst","ssh_sd_0.3","sustr"
) #"yr", "mo"
eval.data<-layers


all_predictions50<-NULL
for( q in 1:numb_models){
predictions_on_newdata <- predict.gbm(model_output[[q]][2][[1]], eval.data, n.trees=model_output[[q]][2][[1]]$gbm.call$best.trees, type="response", family="gaussian")
latlon_predict<-cbind(xy_predict, predictions_on_newdata)
latlon_predict[which(is.na(eval.data$su)==T),3]<-NA #make land NA
all_predictions50<-cbind(all_predictions50,latlon_predict[,3] )
predict_mat<-acast(latlon_predict, x~y, value.var="predictions_on_newdata")  #image.plot(predict_mat)
#image.plot(as.numeric(rownames(predict_mat)), as.numeric(colnames(predict_mat)), predict_mat, xlim=c(-124.5,-121), ylim=c(36,39)) #zoom to study locaiton 
#map(add=T)
}
lon_plot<- as.numeric(rownames(predict_mat)) ; lat_plot<-as.numeric(colnames(predict_mat))

kk<-rowMeans(all_predictions50, na.rm=T)
kk2<-apply(all_predictions50, FUN=sd, 1, na.rm=T)
pred_mat_mean<-fliplr(matrix(kk, nrow=nrow(predict_mat),ncol=ncol(predict_mat)))
pred_mat_sd<-fliplr(matrix(kk2, nrow=nrow(predict_mat),ncol=ncol(predict_mat)))
#image.plot(pred_mat_mean) ; #image.plot(pred_mat_sd)
#if less than zero, make it 0
pred_mat_mean<-replace(pred_mat_mean, pred_mat_mean< 0, 0)
# remove > 150 km from shore
pm<-raster(flipud(t(pred_mat_mean)))
extent(pm)<-extent(Distance_from_shore)
pm<- replace(pm, Distance_from_shore> 150000, NA)
pred_mat_mean<-pm 

ps<-raster(flipud(t(pred_mat_sd)))
extent(ps)<-extent(Distance_from_shore)
ps<- replace(ps, Distance_from_shore> 150000, NA)
pred_mat_sd<-ps

yr_mo<-c(yr_mo, paste(years[i],"-" ,mon,sep=""))
all_predictions_mn <- stack( all_predictions_mn, pred_mat_mean )
all_predictions_sd <- stack( all_predictions_sd, pred_mat_sd )

pred_mat_mean<-replace(pred_mat_mean, pred_mat_mean> max_z_value, max_z_value)
plot( pred_mat_mean) #image.plot( pred_mat_mean)

}
}

#### THIS IS NOW A RASTER STACK 
setwd( model_output_wd) 
names(all_predictions_mn)<- yr_mo
names(all_predictions_sd)<- yr_mo
#save(all_predictions_mn, file= "all_predictions_mn.Rdata")
#save(all_predictions_sd, file= "all_predictions_sd.Rdata")









