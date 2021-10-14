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
path<- "J:/redkite/marcel/"

##################################################################################################################################
#### Red Kite Movebank Data
# pre-filtered in Script "movebank_data.R"
# Filtered by timestamp >"2019-03-23 07:40:00" < "2019-06-20 18:20:00"
# clipped with WKA-radius

redkite<- st_read(paste0(path,"data_download_movebank/gis/redkite_filtered_r_20190323_20190620.shp"))

## Alternative: load CSV:
#redkite<- fread(paste0(path, "data_download_movebank/redkite_filtered_r_20190323_20190620.csv"))
#redkite_shp<-  st_as_sf(redkite, coords = c("utm_east", "utm_north"), crs ="+proj=utm +zone=32 +datum=WGS84 +units=m")
#redkite_shp<-  st_as_sf(redkite, coords = c("utm_east", "utm_north"), crs =4326) #alternative: in case of projection issues
#redkite_shp<- st_transform(obs_shp, crs = 32632)

crs(redkite_shp) # EPSG:32632 = UTM zone 32N
redkite_shp$X<- st_coordinates(redkite_shp)[,1]
redkite_shp$Y<- st_coordinates(redkite_shp)[,2]

WKA_radius<- st_read(paste0(path,"data_QGIS/WKA_radius/WKA_radius_32N.shp"))

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
  st_make_grid(., cellsize = 250, square = T)

grid_9_gps_count <- grid_9 %>% 
  st_intersects(., redkite_shp) %>% 
  lengths(.) 
grid_9_gps_prop<- (grid_9_gps_count / sum(grid_9_gps_count)) * 100
grid_9_gps <-  st_sf(n = grid_9_gps_prop, geometry = st_cast(grid_9, "MULTIPOLYGON"))

mapview(wka_9) + mapview(grid_9_gps) #+ mapview(redkite_shp)

ggplot() +
  geom_sf(data = grid_9_gps, aes(fill=log(n)))+
  geom_sf(data = wka_9, fill="transparent", size =2, col="grey")+
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red")+ # find better gradient; Log-Scale!!!!
  ggtitle("WKA 9 - GPS Rapida")

# Difference of GPS vs. OBS:

grid_9_dif_count<- grid_9_obs_count - grid_9_gps_count

grid_9_dif<- st_sf(n = grid_9_dif_count, geometry = st_cast(grid_9, "MULTIPOLYGON"))
grid_9_dif$nlog<- log(grid_9_obs_count+0.1) - log(grid_9_gps_count+0.1)

ggplot() +
  geom_sf(data = grid_9_dif, aes(fill=nlog))+
  geom_sf(data = wka_9, fill="transparent", size =2, col="grey")+
  scale_fill_gradient2(low = "blue",mid="white",  high = "red")+ 
  geom_sf_text(data = grid_9_dif,aes(label = n))+
  ggtitle("WKA 9 - GPS vs. OBS")

# PROZENT der Daten: Werte jedes Feldes durch Gesamtes Teilen!


#########################################################################

# calculate AKDE for each WKA-radius
#Test


# filter: "Sender" = 'ja' OR "Sender" = 'Sender' OR "Sender" = 'sender'