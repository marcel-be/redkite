library(data.table)
library(dplyr)
library(stringr)
library(sf)
library(raster)
library(ggplot2)
library(mapview)
library(move)
library(ctmm)
library(scales)
path<- "E:/redkite/"

##################################################################################################################################
#### Red Kite Movebank Data
# pre-filtered in Script "movebank_data.R"
# Filtered by timestamp >"2019-03-23 07:40:00" < "2019-06-20 18:20:00"
# clipped with WKA-radius

redkite_shp<- st_read(paste0(path,"data/data_download_movebank/gis/redkite_filtered_r_20190323_20190620.shp"))

## Alternative: load CSV:
#redkite<- fread(paste0(path, "data_download_movebank/redkite_filtered_r_20190323_20190620.csv"))
#redkite_shp<-  st_as_sf(redkite, coords = c("utm_east", "utm_north"), crs ="+proj=utm +zone=32 +datum=WGS84 +units=m")
#redkite_shp<-  st_as_sf(redkite, coords = c("utm_east", "utm_north"), crs =4326) #alternative: in case of projection issues
#redkite_shp<- st_transform(obs_shp, crs = 32632)

crs(redkite_shp) # EPSG:32632 = UTM zone 32N
redkite_shp$X<- st_coordinates(redkite_shp)[,1]
redkite_shp$Y<- st_coordinates(redkite_shp)[,2]

WKA_radius<- st_read(paste0(path,"data/data_QGIS/WKA_radius/WKA_radius_32N.shp"))

mapview(WKA_radius) + mapview(redkite_shp)

## 4.3 create grid

grid<- WKA_radius %>% 
  st_make_grid(., cellsize = 250, square = T) %>% 
  st_intersection(., WKA_radius)

## 4.3 Plots

ggplot() +
  geom_sf(data = WKA_radius, fill="transparent")+
  geom_sf(data = grid, fill="transparent")+
  geom_point(data = redkite_shp, aes(x = X, y = Y), col="orange")


#### 5. Heatmap

pointcount<- grid %>% 
  st_intersects(., redkite_shp) %>% 
  lengths()

pointcount_prop<- (pointcount / sum(pointcount)) * 100

grid_count <- st_sf(n = pointcount_prop, geometry = st_cast(grid, "MULTIPOLYGON"))

mapview(WKA_radius) + mapview(grid_count, zcol = "n")

ggplot() +
  geom_sf(data = WKA_radius, fill="transparent")+
  geom_sf(data = grid_count, aes(fill=n))+
  scale_fill_gradient(low = "white", high = "red")



# subset for one WKA:

wka_9<- WKA_radius %>% 
  filter(WKA_radius$number==9)

grid_9<- wka_9 %>% 
  st_make_grid(., cellsize = 250, square = T) %>% 
  st_intersection(., wka_9)

grid_9_gps_count <- grid_9 %>% 
  st_intersects(., redkite_shp) %>% 
  lengths(.) 
grid_9_gps_prop<- (grid_9_gps_count / sum(grid_9_gps_count)) * 100
grid_9_gps <-  st_sf(n = grid_9_gps_prop, geometry = st_cast(grid_9, "MULTIPOLYGON"))

grid_9_gps$prop_round<- round(grid_9_gps_prop, digits = 2)

mapview(wka_9) + mapview(grid_9_gps) #+ mapview(redkite_shp)

ggplot() +
  geom_sf(data = grid_9_gps, aes(fill=log(n)))+
  geom_sf(data = wka_9, fill="transparent", size =2, col="grey")+
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red")+ # find better gradient; Log-Scale!!!!
  geom_sf_text(data = grid_9_gps,aes(label = prop_round),size=1.54)+
  ggtitle("WKA 9 - GPS Rapida")

## Subsets for all WKAs

for(i in 1:10){
  wka<- WKA_radius %>% 
    filter(WKA_radius$number==i)
  
  grid<- wka %>% 
    st_make_grid(., cellsize = 250, square = T) %>% 
    st_intersection(., wka)
  
  grid_gps_count <- grid %>% 
    st_intersects(., redkite_shp) %>% 
    lengths(.) 
  
  grid_gps_prop<- (grid_gps_count / sum(grid_gps_count)) * 100
  grid_gps <-  st_sf(n = grid_gps_prop, geometry = st_cast(grid, "MULTIPOLYGON"))
  
  grid_gps$prop_round<- round(grid_gps_prop, digits = 2)
  
  png(paste0(path, "output/proportions_gps/","WKA_", WKA_radius$number[i], "_GPS_",WKA_radius$name[i],".png"),
      width=1200, height=1000)
  ggplot() +
    geom_sf(data = grid_gps, aes(fill=log(n)))+
    geom_sf(data = wka, fill="transparent", size =2, col="grey")+
    scale_fill_gradient2(low = "white", mid = "yellow", high = "red")+ # find better gradient; Log-Scale!!!!
    geom_sf_text(data = grid_gps,aes(label = prop_round),size=3.5)+
    ggtitle(paste0("WKA ", WKA_radius$number[i], " - GPS ",WKA_radius$name[i]))
  dev.off()
}


#########################################################################

# calculate AKDE for each WKA-radius


