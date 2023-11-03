load("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/models/combined_models.Rdata")
load("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/models/anchovyGAM.rda")

combined_models
a=combined_models[[1]][[2]]
a=combined_models[1:10]
#1. what is combined models -> do 10 and the average
#2 go over predictors
#- 0.3 sd
#- daily sds and then average
# write out metadata for each variable of how many days there area


file="/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/static_variables/humpback/Grid_Nonrectangle_3km_WEAR_bathy.csv"
grid.pixels    <- read.csv(file, header = TRUE)[, c("lat", "lon180")]
test=rasterFromXYZ(grid.pixels)
test[]=1
