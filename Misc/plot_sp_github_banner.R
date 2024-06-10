library(raster)
library(tidyverse)
library(glue)

outdir="/Users/heatherwelch/Dropbox/OPC_Farallons/misc"

anch=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products_final/rasters/2023/08/anchovy_2023-08-01.grd") %>% 
  rasterToPoints() %>% as.data.frame() %>% rename(values=preds)
bw=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products_final/rasters/2023/08/bluewhale_2023-08-01.grd")%>% 
  rasterToPoints() %>% as.data.frame() %>% rename(values=layer)
hump=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products_final/rasters/2023/08/humpback_2023-08-01.grd")%>% 
  rasterToPoints() %>% as.data.frame() %>% rename(values=preds)
epac=raster("/Users/heatherwelch/Dropbox/OPC_Farallons/operationalization/end_products_final/rasters/2023/08/epac_2023-08-01.grd")%>% 
  rasterToPoints() %>% as.data.frame() %>% rename(values=pred_mean2)


## anchovy ####
plot_title="Anchovy (prob. pres)"
plot_anch=ggplot()+
  geom_tile(data=anch,aes(x = x, y = y, fill=values))+
  scale_fill_gradientn("",colours = pals::parula(100),na.value="black")+
  geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
  theme_classic()+xlab(NULL)+ylab(NULL)+
  coord_sf(xlim = c(-134, -115), ylim = c(30,48),expand=F)+
  ggtitle(glue("{plot_title}"))+
  theme(legend.position = "bottom")

png(glue("{outdir}/anch.png"),width=7,height=9,units='cm',res=400,type = "cairo")
par(ps=10)
# par(mar=c(4,4,1,1))
par(cex=1)
print({plot_anch})
# gg_hm
dev.off()

## epac ####
plot_title="EPAC (CPUE)"
plot_epac=ggplot()+
  geom_tile(data=epac,aes(x = x, y = y, fill=values))+
  scale_fill_gradientn("",colours = pals::parula(100),na.value="black")+
  geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
  theme_classic()+xlab(NULL)+ylab(NULL)+
  coord_sf(xlim = c(-134, -115), ylim = c(30,48),expand=F)+
  ggtitle(glue("{plot_title}"))+
  theme(legend.position = "bottom")

png(glue("{outdir}/epac.png"),width=7,height=9,units='cm',res=400,type = "cairo")
par(ps=10)
# par(mar=c(4,4,1,1))
par(cex=1)
print({plot_epac})
# gg_hm
dev.off()

## hump ####
plot_title="Humpback whale (density)"
plot_hump=ggplot()+
  geom_tile(data=hump,aes(x = x, y = y, fill=values))+
  scale_fill_gradientn("",colours = pals::parula(100),na.value="black")+
  geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
  theme_classic()+xlab(NULL)+ylab(NULL)+
  coord_sf(xlim = c(-134, -115), ylim = c(30,48),expand=F)+
  ggtitle(glue("{plot_title}"))+
  theme(legend.position = "bottom")

png(glue("{outdir}/hump.png"),width=7,height=9,units='cm',res=400,type = "cairo")
par(ps=10)
# par(mar=c(4,4,1,1))
par(cex=1)
print({plot_hump})
# gg_hm
dev.off()

## blue ####
plot_title="Blue whale (prob.pres)"
plot_blue=ggplot()+
  geom_tile(data=bw,aes(x = x, y = y, fill=values))+
  scale_fill_gradientn("",colours = pals::parula(100),na.value="black")+
  geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
  theme_classic()+xlab(NULL)+ylab(NULL)+
  coord_sf(xlim = c(-134, -115), ylim = c(30,48),expand=F)+
  ggtitle(glue("{plot_title}"))+
  theme(legend.position = "bottom")

png(glue("{outdir}/blue.png"),width=7,height=9,units='cm',res=400,type = "cairo")
par(ps=10)
# par(mar=c(4,4,1,1))
par(cex=1)
print({plot_blue})
# gg_hm
dev.off()
