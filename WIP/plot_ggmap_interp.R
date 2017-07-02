library(automap)
library(ggplot2)
library(rgdal)
library(sprawl)

load("/home/lb/Temp/buttami/Mydata.rdata",.GlobalEnv)
canada2<-openshape("/home/lb/Temp/buttami/gpr_000b11a_e.shp")
g <- spTransform(canada2, CRS("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))


coordinates(Mydata)<-~longitude+latitude
proj4string(Mydata)<-CRS("+proj=longlat +datum=NAD83")
sp_mydata<-spTransform(Mydata,CRS(proj4string(g)))

Krig = autoKrige(APPT~1,sp_mydata)$krige_output
Krig = Krig[!is.na(over(Krig,as(g,"SpatialPolygons"))),]  # take only the points falling in poolygons
Krig_df = as.data.frame(Krig)
names(Krig_df) = c("APPT_pred","APPT_var","APPT_stdev","longitude","latitude")
g_fort = fortify(g)
Borders = ggplot() +
  geom_raster(data=Krig_df, aes(x=longitude, y=latitude,fill=APPT_pred))+
  geom_polygon(data=g_fort,aes(x=long,y=lat,group=group),
               fill='transparent',color = "black")+
  theme_bw()
Borders

library(sp)
ptsreg <- spsample(g, 4000, type = "regular")   # Define the ouput grid - 4000 points in polygons extent
Krig = autoKrige(APPT~1,sp_mydata, new_data = ptsreg)$krige_output
Krig = Krig[!is.na(over(Krig,as(g,"SpatialPolygons"))),]  # take only the points falling in poolygons
Krig_df = as.data.frame(Krig)
names(Krig_df) = c("longitude","latitude", "APPT_pred","APPT_var","APPT_stdev")
g_fort = fortify(g)
Borders = ggplot() +
  geom_raster(data=Krig_df, aes(x=longitude, y=latitude,fill=APPT_pred))+
  geom_polygon(data=g_fort,aes(x=long,y=lat,group=group),
               fill='transparent',color = "black")+
  theme_bw()
Borders
